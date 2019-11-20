///
/// An actor message is one of:
///
/// - a `Start` message that instructs an actor to start. The start message includes a reply channel
///   on which the actor must send `Ready` when it is ready to start processing messages.
///
/// - a `Stop` message that instructs an actor to immediately shutdown.
///
/// - A `Msg` with with an underlying message and the mailbox of the sender.
///
pub enum Message {
    case Start(Channel[Ready]),
    case Stop,
    case Msg(Str, Mailbox)
}
