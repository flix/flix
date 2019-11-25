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


  def actorSystem(s: ActorSystem, actors: Map[Str, (InternalActorState, Actor)], control: Channel[SystemEvent]): Unit = {
      match <- control {
        case SystemStart =>
          let currentState = statesOfActors(actors);
          let actors2 = fold Start actors ((actorName, m) -> match Map.get(actorName, actors) {
            case None =>
              // TODO: inconsistent policy, starts a non-existing actor
              actors
            case Some(NotStarted, behavior) =>
              // Start the actor with a new mailbox
              let c = chan SystemEvent 10;
              let actorRef = startActor(c, actorName, behavior);
              let readyChan = chan Ready 0;
              actorRef <- Start(readyChan);
              <- readyChan; // TODO: we block here, but what if the actor never sends Ready? -> select with default? Have an internal actor state that is "started but hasn't replied Ready yet"?
              Map.insert(actorName, (Started(Running, actorRef), behavior), actors)
            case Some(Started(InTransition, actorRef), behavior) =>
              // TODO The actor is waiting for dependencies, but the policy tells us to start it. What should we do in that case? Incosistent policy?
              actors
            case Some(Started(Running, actorRef), behavior) =>
              // TODO Inconsistent policy that tells us to start an actor that is already running
              actors
            case Some(Started(Stopped, actorRef), behavior) =>
              // TODO restart the actor using the same actorRef
              ???
            case Some(Started(ResumablyCrashed, actorRef), behavior) =>
              // TODO resume the actor
              ???
            case Some(Started(NonResumablyCrashed, actorRef), behavior) =>
              // TODO fully restarts the actor
              ???
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

    // TODO: send(sys: RunningActorSystem, actorName: Str): Bool = true if sent to: (1. actor exists (?), 2. actor is running (?))

    ///
    /// Starts a single actor. Returns its reference.
    ///
    pub def startActor(control: Channel[SystemEvent], actorName: Str, behavior: Actor): ActorRef =
        let mailbox = chan Message defaultMailboxCapacity();
        spawn (
          match behavior(mailbox) with {
            case Done =>
              // TODO: use the mailbox as the actor's identity, rather than a name? This would ensure uniqueness (whereas if we compose two actor systems, name clashes are bound to happen)
              control <- ActorDone(actorName)
            case ResumableCrash(_) =>
              control <- ActorResumableCrash(actorName)
            case NonResumableCrash(_) => ??? // TODO
          }
        );
        // TODO: Use a channel to wait for termination.
        // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
        // TODO: what happens when the actor crashes? This should be propagated to the master control channel.
        mailbox

    ///
    /// Initiates a shutdown of the given actor system `s` and waits for its orderly shutdown.
    ///
  pub def shutdown(system: RunningActorSystem): Unit = {
      let c = chan Unit 0;
      system <- SystemShutdown(c);
      <- c
  }
}

