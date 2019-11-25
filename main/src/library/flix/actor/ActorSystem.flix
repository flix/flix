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
type alias RunningActorSystem = Channel[SystemEvent]

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

    def actorSystem(s: ActorSystem, actors: List[(Actor, State)], control: Channel[SystemEvent]): Unit = {
      match <- control {
        case SystemStart =>
            // let runningActors = fold ActorDef Nil ((actor, acc) ->
            //   let (actorName, behavior) = actor;
            //   (actorName, startActor(s.control, actorName, behavior)) :: acc)
            //   (solve (s.startPolicy <+> s.actors)); // TODO: add State relation?
            ???
        case SystemShutdown(c) => ???

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
      let actors = Nil; // TODO: contsruct the map with all actors set to Stopped
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
    //// TODO: can be removed
    //pub def getActor(sys: ActorSystem, actorName: Str): Option[ActorRef] =
    //    Option.map((pair -> snd(pair)),
    //        List.find((pair -> fst(pair) == actorName), sys.runningActors))

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

