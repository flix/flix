///
/// An actor is defined by its behavior, which is a function from a mailbox to an actor result.
///
type alias Actor = Mailbox -> ActorResult[Str]

///
/// An actor is identified by its mailbox
///
type alias ActorRef = Mailbox // TODO: Maybe a name and a mailbox?

///
/// An actor has:
///   - `actorName`: a name identifying the actor
///   - `actor`: the behavior of the actor as a function that takes the current state and mailbox, and returns when the actor is done or has crashed
pub rel ActorDef(actorName: Str, actor: Actor)

// TODO: what is the identity of an actor? Possibilities:
// 1. A name, given as a string. That's really fragile and begs for troubles when e.g., combining actor systems
// 2. A channel. That's probably better, but what if we want multiple actors to use the same channel? Then probably we need a channel + a number, in order to support worker actors that share a channel.
// TODO: Quentin: Why/where do we even need a notation of identity? Can we just not maintain appropriate lists and maps?
//       Answer: identity might be needed for
//         a) relations such as Start, Stop, etc. But maybe not: when we start an actor, do we want to start only one process (mailbox identity), or all its workers (name identity)
//         b) identifying running actors in an ActorSystem that has been started: if we want to shutdown actors, we need to send them a message (mailbox identity)
//       Basically, the identity allows us better control over the actor than with its name. Ideally, it would be a process identifier + mailbox reference.

