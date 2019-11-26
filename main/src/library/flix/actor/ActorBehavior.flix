///
/// An actor behavior is a function from a mailbox to an actor result.
///
type alias ActorBehavior = Mailbox -> ActorResult[Str]
