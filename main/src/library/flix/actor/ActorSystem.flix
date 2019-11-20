///
/// An actor system is a type alias for a record that consists of four fields:
///
/// `actors`          that declares the actors in the system.
/// `startPolicy`     that describes the policy for how actors are started.
/// `restartPolicy`   that describes the policy for how actors are restarted if they crash.
/// `shutdownPolicy`  that describes the policy for how actors are stopped.
///
type alias ActorSystem = {
    actors: ActorPolicy, startPolicy: ActorPolicy, restartPolicy: ActorPolicy, shutdownPolicy: ActorPolicy
}

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
    /// Starts the given actor system `s`. Returns a running actor system.
    ///
    pub def start(s: ActorSystem): RunningActorSystem =
        let control = chan ActorEvent 10;
        { control = control,
          actors = fold ActorDef Nil ((actor, acc) ->
            let (actorName, behavior) = actor;
            (actorName, startActor(control, actorName, behavior)) :: acc)
            (solve (s.startPolicy <+> s.actors)),
          system = s }

    ///
    /// Shuts down the given running actor system `s`.
    ///
    pub def shutdown(_s: RunningActorSystem): Unit =
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

