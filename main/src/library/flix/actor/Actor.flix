///
/// An actor is defined by its behavior, which is a function from a mailbox to an actor result.
///
type alias Actor = Mailbox -> ActorResult[Str]

///
/// An actor is identified by its mailbox
///
type alias ActorRef = Mailbox // TODO: Maybe a name and a mailbox?

///
/// A type alias for the row type of an actor policy.
///
type alias ActorPolicy = #{Start, Stop, ActorDef, MailboxOf, ActorState, DependsOn, Blocked} // TODO: sort by some sane order

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

pub rel MailboxOf(actorName: Str, mailbox: Mailbox) // TODO also just call this Mailbox or maybe use the words "Mailbox" and "Inbox" ?
// TODO: how to populate this relation? Should be done by the actor system somehow...



///
/// Starts a single actor. Returns its reference.
///
pub def startActor(control: Channel[ActorEvent], actorName: Str, behavior: Actor): ActorRef =
    let mailbox = chan Message 10;
    spawn (
      match behavior(mailbox) with {
        case Done =>
          // TODO: use the mailbox as the actor's identity, rather than a name? This would ensure uniqueness (whereas if we compose two actor systems, name clashes are bound to happen)
          control <- ActorStopped(actorName)
        case Crash(_) =>
          control <- ActorCrashed(actorName)
        case FatalCrash(_) => ??? // TODO
      }
    );
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    // TODO: what happens when the actor crashes? This should be propagated to the master control channel.
    mailbox



// pub def react(_system: RunningActorSystem): Unit =
//     // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
//     ()

///
/// The Grand TODO List
///
/// Unassigned:
/// - Add support for aggregation.
/// - Implement framework
/// - Implement examples
/// - is it possible to support impredicate types?
///
/// Magnus:
/// - Introduce explicit support for lattices.
/// - Refactor types of relations and lattices :(
///   - Refactor the code not to depend on relations and lattices maps.
///
/// Quentin:
/// - Add a master control channel
