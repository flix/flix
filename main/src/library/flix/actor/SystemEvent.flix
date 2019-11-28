///
/// An internal data type of events used to manage the actor system.
///
/// Note: Must not be used directly by clients of the actor library.
///
pub enum SystemEvent {
    ///
    /// A message that represents that the actor has started and is ready to receive messages.
    ///
    /// @param the name of the actor.
    ///
    case ActorReady(Str),

    ///
    /// A message that represents that an actor has terminated normally.
    ///
    /// @param the name of the actor.
    ///
    case ActorDone(Str),

    ///
    /// A message that represents that the actor has crashed, but it can still be resumed.
    ///
    /// @param the name of the actor.
    ///
    case ActorResumableCrash(Str, ActorBehavior),

    ///
    /// A message that represents that the actor has crashed and cannot be resumed.
    ///
    /// @param the name of the actor.
    ///
    case ActorNonResumableCrash(Str),

    ///
    /// A command that the actor system should start.
    /// TODO: should there be a Channel[Unit] to notify that the start has completed as well?
    ///
    case SystemStart,

    ///
    /// A command that the actor system should shutdown.
    ///
    /// @param the channel on which the `Unit` value is sent once the shutdown has completed.
    ///
    case SystemShutdown(Channel[Unit]),

    ///
    /// A command to send a message to a running actor.
    ///
    /// @param name: the name of the actor
    /// @param msg: the message to send
    /// @param c: the reply channel
    ///
    case SendMessage(Str, Message, Channel[Bool])
}
