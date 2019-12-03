///
/// An actor directory, used to retrieve mailboxes of actors in the system
///
type alias ActorDirectory = Unit // TODO: define and move to a different file

///
/// An actor behavior is a function from a mailbox to an actor result.
///
type alias ActorBehavior = ActorDirectory -> Mailbox -> ActorResult[Str]
