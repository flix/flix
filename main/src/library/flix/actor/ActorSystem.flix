///
/// An actor system is a type alias for a record that consists of four fields:
///
/// `actors`          that declares the actors in the system.
/// `startPolicy`     that describes how actors are started.
/// `restartPolicy`   that describes the actions taken when an actor crashes.
/// `shutdownPolicy`  that describes how actors are stopped.
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

}

