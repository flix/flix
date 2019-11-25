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
type alias RunningActorSystem = Channel[SystemEvent] // TODO: Maybe make this an enum or opaque type.

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

    def statesOfActors(actors: Map[Str, (InternalActorState, Actor)]): ActorPolicy =
      Map.foldLeftWithKey((actorName, states, actor) ->
        match fst(actor) {
          case NotStarted => states <+> #{ActorState(actorName, Stopped).}
          case Started(st, _) => states <+> #{ActorState(actorName, st).}
        }, #{}, actors)


  ///
  /// Starts an actor given its name, behavior, and mailbox. Returns the updated map of running actors
  ///
  def startActor(actors: Map[Str, (InternalActorState, Actor)], control: Channel[SystemEvent], actorName: Str, behavior: Actor, mailbox: Channel[Message]): Map[Str, (InternalActorState, Actor)] = {
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
  /// The main loop of the actor system, handling messages over the control channel
  ///
  def actorSystem(s: ActorSystem, actors: Map[Str, (InternalActorState, Actor)], control: Channel[SystemEvent]): Unit = {
      match <- control {
        case SystemStart =>
          let currentState = statesOfActors(actors);
          let actors2 = fold Start actors ((actorName, m) -> match Map.get(actorName, actors) {
            case None =>
              // Inconsistent policy, starts a non-existing actor. This is a no-op.
              actors
            case Some(NotStarted, behavior) =>
              // <no-state>, Start => start the actor
              // Start the actor with a new mailbox
              let mailbox = chan Message defaultMailboxCapacity();
              startActor(actors, control, actorName, behavior, mailbox)
            case Some(Started(InTransition, mailbox), behavior) =>
              // InTransition, Start => no-op
              actors
            case Some(Started(Running, mailbox), behavior) =>
              // Running, Start => no-op
              actors
            case Some(Started(Stopped, mailbox), behavior) =>
              // Stopped, Start => start the actor (reusing the mailbox)
              startActor(actors, control, actorName, behavior, mailbox)
            case Some(Started(ResumablyCrashed, mailbox), behavior) =>
              // ResumablyCrashed, Start => restart the actor (with initial state)
              startActor(actors, control, actorName, behavior, mailbox)
            case Some(Started(NonResumablyCrashed, mailbox), behavior) =>
              // NonResumablyCrashed, Start => start the actor
              startActor(actors, control, actorName, behavior, mailbox)
           }) (solve (s.startPolicy <+> s.actors <+> currentState));
          actorSystem(s, actors2, control)
        case SystemShutdown(c) => ???
        case SendMessage(actorName, msg, c) =>
          // TODO: results in "head of empty list" exception in PatternExhaustiveness phase
          // match Map.get(actorName, actors) {
          //   case Some((Started(_, actorRef), _)) =>
          //     actorRef <- msg;
          //     c <- true
          //   case None => c <- false // Actor not found
          //   case Some(_) => c <- false // Actor not running
          // };
          actorSystem(s, actors, control)
        case ActorReady(name) => ???
        case ActorDone(name) => ???
        case ActorResumableCrash(name) => ???
        case ActorNonResumableCrash(name) => ???
      }
    }

    ///
    /// Starts the given actor system `s`. Returns the updated actor system
    ///
    pub def start(s: ActorSystem): RunningActorSystem = {
      let control = chan SystemEvent 10;
      let actors = fold ActorDef Map#{} ((actor, m) ->
            let (actorName, behavior) = actor;
            Map.insert(actorName, (NotStarted, behavior), m))
            (solve s.actors);
      spawn (actorSystem(s, actors, control));
      control <- SystemStart;
      control
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
    pub def send(sys: RunningActorSystem, actorName: Str, msg: Message): Bool = {
        let c = chan Bool 0;
        sys <- SendMessage(actorName, msg, c);
        <- c
    }

    ///
    /// Initiates a shutdown of the given actor system `s` and waits for its orderly shutdown.
    ///
  pub def shutdown(system: RunningActorSystem): Unit = {
      let c = chan Unit 0;
      system <- SystemShutdown(c);
      <- c
  }
}

