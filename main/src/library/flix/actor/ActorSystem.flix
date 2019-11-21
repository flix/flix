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

    /// The following fields are runtime-related.
    control: Channel[ActorEvent],
    // TODO: now the running actors are stored as (actorName, mailbox), but maybe we want the state as well? That will be useful when we need to construct the ActorState relation.
    runningActors: List[(Str, ActorRef)]
}

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
    /// Shuts down the given running actor system `s`.
    ///
    pub def shutdown(_s: ActorSystem): Unit =
      // TODO: fails to compile due to not being able to unify Str with (Str) (a 1-tuple)
      //let stopped = fold Stop Nil ((actorName, acc) ->
      //  match getActor(system, actorName) {
      //    case Some(actorRef) =>
      //      // TODO: What if the actor just ignores this message? Or if it crashes when stopping?
      //      actorRef <- Stop;
      //      actorName :: acc
      //    case None =>
      //      Console.printLine("Actor not found: " + actorName);
      //      acc
      //  }) (solve (system.system.shutdownPolicy /* TODO: this should be the composed with current status of the ActorState relation */));
      //if (List.length(stopped) == 0) {
      //  ()
      //} else {
      //  // TODO: wait a bit before recursing?
      //  // TODO: keep the current status of the ActorState relation somewhere
      //  shutdown(system)
      //}
      ()

}

