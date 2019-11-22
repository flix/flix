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
    case ActorDone(Str),

    ///
    /// Indicates that the actor has crashed, but it can still be resumed.
    ///
    /// @param the name of the actor.
    ///
    case ActorResumableCrash(Str),

    ///
    /// Indicates that the actor has crashed and cannot be resumed.
    ///
    /// @param the name of the actor.
    ///
    case ActorNonResumableCrash(Str)

}
