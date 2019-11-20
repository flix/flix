///
/// An internal event used to manage the actor system.
///
/// Not to be used by the clients of the actor library.
///
pub enum ActorEvent {
    ///
    /// Indicates that the actor has started and is ready to receive messages.
    ///
    /// @param the name of the actor.
    ///
    case ActorReady(Str),

    ///
    /// Indicates that an actor has terminated normally.
    ///
    /// @param the name of the actor.
    ///
    case ActorStopped(Str),

    ///
    /// Indicates that the actor has crashed.
    ///
    /// @param the name of the actor.
    ///
    case ActorCrashed(Str),

    ///
    /// Indicates that the actor has crashed and cannot be restarted.
    ///
    /// @param the name of the actor.
    ///
    case ActorFinal(Str)

}

// TODO: Since this is private, maybe move it into a more appropriate namespace?
// TODO: Naming should be made more consistent with ActorResult.
