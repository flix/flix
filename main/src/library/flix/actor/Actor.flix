///
/// An actor is a function from a mailbox to an actor result.
///
type alias Actor = Mailbox -> ActorResult[Str]
