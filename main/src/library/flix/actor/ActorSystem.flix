///
/// An actor system is a type alias for a record that consists of four fields:
///
/// @field `actors`          that declares the actors in the system.
/// @field `startPolicy`     that describes the policy for how actors are started.
/// @field `restartPolicy`   that describes the policy for how actors are restarted if they crash.
/// @field `shutdownPolicy`  that describes the policy for how actors are stopped.
/// @field `control`         the runtime control channel of this actor system
/// @field `actors`          the actors that are running
///
type alias ActorSystem = {
    actors: ActorPolicy,
    startPolicy: ActorPolicy,
    restartPolicy: ActorPolicy,
    shutdownPolicy: ActorPolicy
}

///
/// A running actor system is a type alias for a channel of actor events.
///
opaque type RunningActorSystem = Channel[SystemEvent]

///
/// An actor is identified by its mailbox
///
type alias ActorRef = Mailbox // TODO: Quentin: Maybe a name and a mailbox?

namespace ActorSystem {

    ///
    /// Returns an actor system with a default configuration.
    ///
    pub def default(): ActorSystem = {
        actors          = #{},
        startPolicy     = defaultStartPolicy(),
        shutdownPolicy  = defaultShutdownPolicy(),
        restartPolicy   = defaultRestartPolicy()
    }

    ///
    /// Returns the default mailbox capacity.
    ///
    pub def defaultMailboxCapacity(): Int = 1000 * 1000

    enum InternalActorState {
      ///
      /// An actor that has not been started previously, and has no mailbox
      ///
      case NotStarted,
      ///
      /// An actor that has been started and been assigned a mailbox. The actor could be stopped or crashed now.
      ///
      case Started(State, ActorRef)
    }

    def statesOfActors(actors: Map[Str, (InternalActorState, ActorBehavior)]): ActorPolicy =
      Map.foldLeftWithKey((actorName, states, actor) ->
        match fst(actor) {
          case NotStarted => states <+> #{ActorState(actorName, Stopped).}
          case Started(st, _) => states <+> #{ActorState(actorName, st).}
        }, #{}, actors)


  ///
  /// Starts an actor given its name, behavior, and mailbox. Returns the updated map of running actors
  ///
  def startActor(actors: Map[Str, (InternalActorState, ActorBehavior)], control: Channel[SystemEvent], actorName: Str, behavior: ActorBehavior, mailbox: Channel[Message]): Map[Str, (InternalActorState, ActorBehavior)] = {
    // Create the process of the new actor
    spawn (
        match behavior(mailbox) with {
            case Done =>
                // Notify on the control channel that the actor is done
                control <- ActorDone(actorName)
            case ResumableCrash(_) =>
                // Notify that the actor has crashed
                control <- ActorResumableCrash(actorName)
            case NonResumableCrash(_) =>
                // Notify that the actor has crashed
                control <- ActorNonResumableCrash(actorName)
        }
    );
    // The channel on which the actor will notify us that is has successfully initialized
    let readyChan = chan Ready 0;
    // Tell the actor to initialize
    mailbox <- Start(readyChan);
    spawn ({
      // Wait (on a different process) for the actor to fully initialize
      <- readyChan; // TODO: what if the actor crashes before being ready? Should we use a timeout?
      // Then notify the actor system
      control <- ActorReady(actorName)
    });
    // Add the actor to the map of running actors. It is now in transition.
    Map.insert(actorName, (Started(InTransition, mailbox), behavior), actors)
  }

  ///
  /// Runs the start policy of an actor system. Returns the updated map of running actors
  ///
  def runStartPolicy(s: ActorSystem, actors: Map[Str, (InternalActorState, ActorBehavior)], control: Channel[SystemEvent]): Map[Str, (InternalActorState, ActorBehavior)] =
    let currentState = statesOfActors(actors);
    fold Start actors ((actorName, m) -> match Map.get(actorName, m) {
      case Some((NotStarted, behavior)) =>
        // <no-state>, Start => start the actor
        // Start the actor with a new mailbox
        let mailbox = chan Message defaultMailboxCapacity();
        startActor(m, control, actorName, behavior, mailbox)
      case Some((Started(Stopped, mailbox), behavior)) =>
        // Stopped, Start => start the actor (reusing the mailbox)
        startActor(m, control, actorName, behavior, mailbox)
      case Some((Started(ResumablyCrashed, mailbox), behavior)) =>
        // ResumablyCrashed, Start => restart the actor (with initial state)
        startActor(m, control, actorName, behavior, mailbox)
      case Some((Started(NonResumablyCrashed, mailbox), behavior)) =>
        // NonResumablyCrashed, Start => start the actor
        startActor(m, control, actorName, behavior, mailbox)
      case _ =>
        // Other cases are no-ops
        m
    }) (solve (s.startPolicy <+> s.actors <+> currentState))

  ///
  /// Runs the shutdown policy of an actor system. Returns when the system has fully terminated.
  ///
  def runShutdownPolicy(s: ActorSystem, actors: Map[Str, (InternalActorState, ActorBehavior)], _control: Channel[SystemEvent]): Map[Str, (InternalActorState, ActorBehavior)] =
     // TODO: this does not wait for everything to be stopped, nor does it even make sure that all stop messages have been sent (e.g., in case of dependencies)
    let currentState = statesOfActors(actors);
    fold Stop actors ((actorName, m) -> match Map.get(actorName, m) {
      case Some((Started(Running, mailbox), _)) =>
         // Running, Stop => stop the actor
         mailbox <- Stop; // actor will terminate and will notify the actor system
         m
      case Some((Started(ResumablyCrashed, mailbox), behavior)) =>
        // ResumablyCrashed, Stop => change state to stopped
        Map.insert(actorName, (Started(Stopped, mailbox), behavior), m)
      case Some((Started(NonResumablyCrashed, mailbox), behavior)) =>
        // NonResumablyCrashed, Stop => change state to stopped
        Map.insert(actorName, (Started(Stopped, mailbox), behavior), m)
      case _ =>
        // Other cases are no-ops
        m
    }) (solve (s.shutdownPolicy <+> s.actors <+> currentState))

  ///
  /// The main loop of the actor system, handling messages over the control channel
  ///
  def actorSystem(s: ActorSystem, actors: Map[Str, (InternalActorState, ActorBehavior)], control: Channel[SystemEvent]): Unit = {
      match <- control {
        case SystemStart =>
          // TODO: this runs the start policy only once. But if some actors need others to be running first, this won't start them.
          let actors2 = runStartPolicy(s, actors, control);
          actorSystem(s, actors2, control)
        case SystemShutdown(c) =>
          let actors2 = runShutdownPolicy(s, actors, control);
          actorSystem(s, actors2, control)
        case SendMessage(actorName, msg, c) =>
          match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), _)) =>
              mailbox <- msg;
              c <- true
            case Some(_) => c <- false // Actor not running
            case _ => c <- false // Actor not found
          };
          actorSystem(s, actors, control)
        case ActorReady(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(InTransition, mailbox), behavior)) =>
              // The actor has initialized and is now running
              Map.insert(actorName, (Started(Running, mailbox), behavior), actors)
            case _ =>
              // TODO
              ???
          };
          // TODO: use running policy here
          actorSystem(s, actors2, control)
        case ActorDone(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), behavior)) =>
              Map.insert(actorName, (Started(Stopped, mailbox), behavior), actors)
            case _ =>
              // TODO: When do these other case happen? If the actor is done, it should not be in any other state than started.
              ???
          };
          // TODO: use running policy here
          actorSystem(s, actors2, control)
        case ActorResumableCrash(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(_, mailbox), behavior)) =>
              Map.insert(actorName, (Started(ResumablyCrashed, mailbox), behavior), actors)
            case _ =>
              // TODO: actor should only crash if it was running before
              // TODO: what about InTransition? An actor could crash while initializing
              // TODO: how to store resumable state?
              ???
          };
          // TODO: use running policy here
          actorSystem(s, actors2, control)
        case ActorNonResumableCrash(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), behavior)) =>
              Map.insert(actorName, (Started(NonResumablyCrashed, mailbox), behavior), actors)
            case _ =>
              // TODO: same as for resumable crash
              ???
          };
          // TODO: use running policy here
          actorSystem(s, actors2, control)
      }
    }

    ///
    /// Starts the given actor system `s`. Returns the updated actor system
    ///
    pub def start(s: ActorSystem): RunningActorSystem = {
      let control = chan SystemEvent 10;
      let actors = fold Actor Map#{} ((actor, m) ->
            let (actorName, behavior) = actor;
            Map.insert(actorName, (NotStarted, behavior), m))
            (solve s.actors);
      spawn (actorSystem(s, actors, control));
      control <- SystemStart;
      RunningActorSystem(control)
    }

    // let stopped = fold Stop Nil ((actorName, acc) ->
          //   match getActor(system, actorName) {
          //     case Some(actorRef) =>
          //       // TODO: What if the actor just ignores this message? Or if it crashes when stopping? The actor behaviour should return Done. We can wait for this on some internal channel maybe?
          // Magnus: Let us not worry about badly behaved actors or timeouts for now. We can add that later.
          //       actorRef <- Stop;
          //       actorName :: acc
          //     case None =>
          //       Console.printLine("Actor not found: " + actorName);
          //       acc
          //   }) (solve (system.shutdownPolicy /* TODO: this should be the composed with current status of the ActorState relation */));
          // if (List.length(stopped) == 0) {
          //   ()
          // } else {
          //   // TODO: wait a bit before recursing?
          //   // TODO: keep the current status of the ActorState relation somewhere
          //   shutdown(system)
          // }

    ///
    /// Returns the reference of an actor in an actor system, given its name.
    ///
    pub def send(s: RunningActorSystem, actorName: Str, msg: Message): Bool = {
        let RunningActorSystem(sys) = s;
        let c = chan Bool 0;
        sys <- SendMessage(actorName, msg, c);
        <- c
    }

    ///
    /// Initiates a shutdown of the given actor system `s` and waits for its orderly shutdown.
    ///
  pub def shutdown(s: RunningActorSystem): Unit = {
        let RunningActorSystem(sys) = s;
        let c = chan Unit 0;
        sys <- SystemShutdown(c);
        <- c
  }
}

