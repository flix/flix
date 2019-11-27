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

    type alias ActorMap = Map[Str, (InternalActorState, ActorBehavior)]

    def statesOfActors(actors: ActorMap): ActorPolicy =
      Map.foldLeftWithKey((actorName, states, actor) ->
        match fst(actor) {
          case NotStarted => states <+> #{ActorState(actorName, Stopped).}
          case Started(st, _) => states <+> #{ActorState(actorName, st).}
        }, #{}, actors)


  ///
  /// Starts an actor given its name, behavior, and mailbox. Returns the updated map of running actors
  ///
  def startActor(actors: ActorMap, control: Channel[SystemEvent], actorName: Str, behavior: ActorBehavior, mailbox: Channel[Message]): ActorMap = {
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
  def runStartPolicy(s: ActorSystem, actors: ActorMap, control: Channel[SystemEvent]): ActorMap = {
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
  }

  ///
  /// Runs the shutdown policy of an actor system. Directly returns the updated actor map after one iteration of the policy.
  ///
  def runShutdownPolicy(s: ActorSystem, actors: ActorMap, _control: Channel[SystemEvent]): ActorMap = {
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
  }

  /// If an actor policy specifies multiple actions for the same actor then the final action is computed by join (which is symmetric)
  ///
  /// Start, Stop   => Start
  /// Stop, Resume  => Resume
  /// Start, Resume => Resume
  /// x, x          => x
  ///


  // TODO: this is only used by runRestartPolicy, is there a better way to do it?
  enum ActorAction {
    case ActionResume,
    case ActionStart,
    case ActionStop
  }
  def runRestartPolicy(s: ActorSystem, actors: ActorMap, control: Channel[SystemEvent]): ActorMap = {
    let currentState = statesOfActors(actors);
    // We just accumulate for each actor, whether it should 1. Stop, 2. Start, 3. Resume, overriding any previous value. This corresponds to the increasing order of the sop/start/resume lattice.
    let m0: Map[Str, ActorAction] = Map#{};
    let policy = solve (s.restartPolicy <+> currentState);
    let m1 = fold Stop m0 ((actorName, m) -> Map.insert(actorName, ActionStop, m)) policy;
    let m2 = fold Start m1 ((actorName, m) -> Map.insert(actorName, ActionStart, m)) policy;
    let m3 = fold Resume m2 ((actorName, m) -> Map.insert(actorName, ActionResume, m)) policy;
    // Then, we just apply the action for each actor, according to the table.
    Map.foldLeftWithKey((actorName, m, action) ->
      match action {
      case ActionStart => match Map.get(actorName, m) {
          case Some((NotStarted, behavior)) =>
            // Start the actor with a new mailbox
            let mailbox = chan Message defaultMailboxCapacity();
            startActor(m, control, actorName, behavior, mailbox)
          case Some((Started(Stopped, mailbox), behavior)) =>
            // Start the actor, reusing the mailbox
            startActor(m, control, actorName, behavior, mailbox)
          case Some((Started(ResumablyCrashed, mailbox), behavior)) =>
            // Start the actor with the initial state, but reusing the mailbox
            startActor(m, control, actorName, behavior, mailbox)
          case Some((Started(NonResumablyCrashed, mailbox), behavior)) =>
            // Start the actor with the initial state, reusing the mailbox
            startActor(m, control, actorName, behavior, mailbox)
          case _ =>
            // No-op
            m
      }
      case ActionResume => match Map.get(actorName, m) {
          case Some((Started(Stopped, mailbox), behavior)) =>
            // Start the actor with the initial state, reusing the mailbox
            startActor(m, control, actorName, behavior, mailbox)
          case Some((Started(ResumablyCrashed, mailbox), behavior)) =>
            // TODO: resume the actor (here we start from the initial state)
            startActor(m, control, actorName, behavior, mailbox)
          case Some((Started(NonResumablyCrashed, mailbox), behavior)) =>
            // Start the actor with initial state
            startActor(m, control, actorName, behavior, mailbox)
          case _ =>
            // No-op
            m
      }
      case ActionStop => match Map.get(actorName, m) {
          case Some((Started(Running, mailbox), _)) =>
            // Stop the actor
            mailbox <- Stop; // actor will terminate and will notify the actor system
            m
          case Some((Started(ResumablyCrashed, mailbox), behavior)) =>
            // Change the state of the crashed actor to stopped
            Map.insert(actorName, (Started(Stopped, mailbox), behavior), m)
          case Some((Started(NonResumablyCrashed, mailbox), behavior)) =>
            // Change the state of the crashed actor to stopped
            Map.insert(actorName, (Started(Stopped, mailbox), behavior), m)
          case _ =>
            // No-op
            m
      }
    }, actors, m3)
  }

  enum Policy {
    case PolicyNotStarted, // Not started yet
    case PolicyStarting, // Currently starting
    case PolicyRunning, // Fully running
    case PolicyStopping // Currently stopping
  }

  def runPolicy(s: ActorSystem, actors: ActorMap, control: Channel[SystemEvent], policy: Policy): ActorMap =
    let _x = PolicyRunning; // TODO: remove
    match policy {
      case PolicyNotStarted => actors
      case PolicyStarting => runStartPolicy(s, actors, control)
      case PolicyRunning => runRestartPolicy(s, actors, control)
      case PolicyStopping => runShutdownPolicy(s, actors, control)
    }

  ///
  /// The main loop of the actor system, handling messages over the control channel
  ///
  def actorSystem(s: ActorSystem, actors: ActorMap, control: Channel[SystemEvent], policy: Policy): Unit = {
      match <- control {
        case SystemStart =>
          assert!(policy == PolicyNotStarted); // Otherwise, the actor system has been started more than once.
          actorSystem(s, runPolicy(s, actors, control, PolicyStarting), control, PolicyStarting)
        case SystemShutdown(c) =>
          actorSystem(s, runPolicy(s, actors, control, PolicyStopping), control, PolicyStopping)
        case SendMessage(actorName, msg, c) =>
          match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), _)) =>
              mailbox <- msg;
              c <- true
            case Some(_) => c <- false // Actor not running
            case _ => c <- false // Actor not found
          };
          actorSystem(s, actors, control, policy)
        case ActorReady(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(InTransition, mailbox), behavior)) =>
              // The actor has initialized and is now running
              // TODO: if all actors are running, we can move to a "running" policy
              Map.insert(actorName, (Started(Running, mailbox), behavior), actors)
            case _ =>
              // TODO
              ???
          };
          actorSystem(s, runPolicy(s, actors2, control, policy), control, policy)
        case ActorDone(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), behavior)) =>
              Map.insert(actorName, (Started(Stopped, mailbox), behavior), actors)
            case _ =>
              // TODO: When do these other case happen? If the actor is done, it should not be in any other state than started.
              ???
          };
          actorSystem(s, runPolicy(s, actors2, control, policy), control, policy)
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
          actorSystem(s, runPolicy(s, actors2, control, policy), control, policy)
        case ActorNonResumableCrash(actorName) =>
          let actors2 = match Map.get(actorName, actors) {
            case Some((Started(Running, mailbox), behavior)) =>
              Map.insert(actorName, (Started(NonResumablyCrashed, mailbox), behavior), actors)
            case _ =>
              // TODO: same as for resumable crash
              ???
          };
          actorSystem(s, runPolicy(s, actors2, control, policy), control, policy)
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
      spawn (actorSystem(s, actors, control, PolicyNotStarted));
      control <- SystemStart;
      RunningActorSystem(control)
    }

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

