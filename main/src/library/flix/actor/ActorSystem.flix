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
    shutdownPolicy: ActorPolicy,

    // TODO: Quentin: I think we should separate actor systems from running actor systems.
    // TODO: Quentin: We could use extensible records, if you prefer. Then a RunningActor system is like an actor system with additional fields.

    /// The following fields are runtime-related.
    control: Channel[ActorEvent],
    // TODO: now the running actors are stored as (actorName, mailbox), but maybe we want the state as well? That will be useful when we need to construct the ActorState relation.
    runningActors: List[(Str, ActorRef)] // TODO: Quentin: How can you maintain this immutable field when the state of the system changes? Hmm... I think we need our own recursive function that maintains the state.
}

///
/// An actor is identified by its mailbox
///
type alias ActorRef = Mailbox // TODO: Maybe a name and a mailbox?

// TODO: The identity of a running actor should probably be its name + mailbox.

namespace ActorSystem {

    ///
    /// Returns an actor system with a default configuration.
    ///
    pub def default(): ActorSystem = {
        actors          = #{},
        startPolicy     = defaultStartPolicy(),
        shutdownPolicy  = defaultShutdownPolicy(),
        restartPolicy   = defaultRestartPolicy(),
        control         = chan ActorEvent 10,
        runningActors   = Nil
    }

    ///
    /// Returns the default mailbox capacity.
    ///
    pub def defaultMailboxCapacity(): Int = 1000 * 1000

    ///
    /// Starts the given actor system `s`. Returns the updated actor system
    ///
    pub def start(s: ActorSystem): ActorSystem = {
        runningActors = fold ActorDef Nil ((actor, acc) ->
            let (actorName, behavior) = actor;
            (actorName, startActor(s.control, actorName, behavior)) :: acc)
            (solve (s.startPolicy <+> s.actors))
        | s
    }

    ///
    /// Returns the reference of an actor in an actor system, given its name.
    ////
    pub def getActor(sys: ActorSystem, actorName: Str): Option[ActorRef] =
        Option.map((pair -> snd(pair)),
            List.find((pair -> fst(pair) == actorName), sys.runningActors))

    ///
    /// Starts a single actor. Returns its reference.
    ///
    pub def startActor(control: Channel[ActorEvent], actorName: Str, behavior: Actor): ActorRef =
        let mailbox = chan Message defaultMailboxCapacity();
        spawn (
          match behavior(mailbox) with {
            case Done =>
              // TODO: use the mailbox as the actor's identity, rather than a name? This would ensure uniqueness (whereas if we compose two actor systems, name clashes are bound to happen)
              control <- ActorStopped(actorName)
            case Crash(_) =>
              control <- ActorCrashed(actorName)
            case FatalCrash(_) => ??? // TODO
          }
        );
        // TODO: Use a channel to wait for termination.
        // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
        // TODO: what happens when the actor crashes? This should be propagated to the master control channel.
        mailbox

    ///
    /// Shuts down the given running actor system `s`.
    ///
    pub def shutdown(system: ActorSystem): Unit =
      let stopped = fold Stop Nil ((actorName, acc) ->
        match getActor(system, actorName) {
          case Some(actorRef) =>
            // TODO: What if the actor just ignores this message? Or if it crashes when stopping? The actor behaviour should return Done. We can wait for this on some internal channel maybe?
            actorRef <- Stop;
            actorName :: acc
          case None =>
            Console.printLine("Actor not found: " + actorName);
            acc
        }) (solve (system.shutdownPolicy /* TODO: this should be the composed with current status of the ActorState relation */));
      if (List.length(stopped) == 0) {
        ()
      } else {
        // TODO: wait a bit before recursing?
        // TODO: keep the current status of the ActorState relation somewhere
        shutdown(system)
      }
}

