///////////////////////////////////////////////////////////////////////////////
/// Actor System                                                             //
///////////////////////////////////////////////////////////////////////////////

///
/// An actor is a function that returns an actor result.
///
/// An actor result is either `Done` or `Crash(s, e)`.
///
/// An actor must return the `Done` value when it successfully completes its work.
/// An actor may return the `Crash(s, e)` value when it is unable to complete its work and wants to abort.
/// The `s` value represents the last known good state and `e` represents an error value.
///
/// @param s the type of state.
/// @param e the type of the error.
///
pub enum ActorResult[e] {
    ///
    /// Indicates that the actor successfully completed its work.
    ///
    case Done,

    ///
    /// Indicates that the actor was unable to complete it works and aborted.
    ///
    case Crash(e)
}

///
/// An actor, while running, communicates with the actor system by sending control messages.
///
/// An actor must send the `Ready` message when it has started and is ready to receive messages.
///
pub enum ControlMsg {
    ///
    /// Indicates that the actor has started and is ready to receive messages.
    ///
    case Ready(Str),

    ///
    /// Indicates that an actor has crashed
    ///
    case Crashed(Str),

    ///
    /// Indicates that an actor has successfully terminated
    ///
    case Terminated(Str)
}


/// An actor receives messages of type ActorMsg.
/// These can either be a message indicating to start (`Start`) or stop (`Stop`) the actor, or an actuall application-level message (`Msg`).
/// Upon reception of a `Start` message, the actor has to reply on the given channel with a `Ready` message.
/// Upon reception of `Stop` message, the actor has to ends its execution. TODO: does it? If not, what happens?
/// When receiving a `Msg` message, the actor can reply to the sender of the message using the argument channel.

// TODO: Do we want two types of messages depending on whether we need a reply channel?
// Q: I would not have two types of messages, otherwise it is not clear from the sender what to send (imagine an actor that uses the control channel and then changes its behaviour to not using the control channel)
pub enum ActorMsg {
    case Start(Channel[ControlMsg]),
    case Stop,
    case Msg(Mailbox, Str)
}

///
/// A mailbox is a type alias for a channel of actor messages.
///
type alias Mailbox = Channel[ActorMsg]

///
/// An actor is identifiedby its mailbox
///
type alias ActorRef = Mailbox

// TODO: Introduce an use the definition of Actor in the definition of Actor.

///
/// An actor system is a record that consists of four fields:
///
/// A `actors`          field that declares the actors in the system.
/// A `startPolicy`     field that describes how to start the actors.
/// A `shutdownPolicy`  field that describes how to stop the actors.
/// A `restartPolicy`   field that describes the actions to take when an actor crashes.
///
type alias ActorSystem = {actors: #{ActorDef, Start, Started, MailboxOf}, startPolicy: #{Start, Started, ActorDef, MailboxOf}, shutdownPolicy: #{Stop}, restartPolicy: #{Start, Crashed}}

///
/// Returns an actor system with the default configuration.
///
pub def defaultActorSystem(): ActorSystem = {
    actors          = #{},
    startPolicy     = defaultStartPolicy(),
    shutdownPolicy  = defaultShutdownPolicy(),
    restartPolicy   = defaultRestartPolicy()
}

///
/// Returns the default start policy.
///
pub def defaultStartPolicy(): #{Start, Started, ActorDef, MailboxOf} = #{
    // TODO
    Start(actorRef) :- ActorDef(actorRef, _).
}

///
/// Returns the default shutdown policy.
///
pub def defaultShutdownPolicy(): #{Stop} = #{
    // TODO
}


///
/// Returns the default restart policy.
///
pub def defaultRestartPolicy(): #{Start, Crashed} = #{
    // TODO
    Start(actorRef) :- Crashed(actorRef).
}





/// An actor has:
///   - `actorName`: a name identifying the actor
///   - `mailbox`: a channel where the actor receives messages from
///   - `actor`: the behavior of the actor as a function that takes the current state and mailbox, and returns when the actor is done or has crashed
/// TODO: initial state
pub rel ActorDef(actorName: Str, actor: Mailbox -> ActorResult[Str])

// TODO: what is the identity of an actor? Possibilities:
// 1. A name, given as a string. That's really fragile and begs for troubles when e.g., combining actor systems
// 2. A channel. That's probably better, but what if we want multiple actors to use the same channel? Then probably we need a channel + a number, in order to support worker actors that share a channel.

pub rel Started(actorName: Str)
pub rel Stopped(actorName: Str)
pub rel Crashed(actorName: Str)

pub rel Start(actorName: Str)
pub rel Stop(actorName: Str)

pub rel MailboxOf(actorName: Str, mailbox: Mailbox)
// TODO: how to populate this relation?

// TODO: We dont really have proper support for type aliases, we might want to fix that.

// TODO: Define core functions here
// -----------------------------------------------------------------------------------------------

type alias ControlChannel = Channel[ControlMsg]

///
/// Starts an actor system `a`. Returns a channel to control this actor system.
///
pub def start(a: ActorSystem): ControlChannel =
    let control = chan ControlMsg 10;
    // TODO: We lack pattern matching on records.
    let startPolicy = a.startPolicy <+> a.actors;
    let _actors = fold ActorDef Nil ((actor, acc) ->
      let (actorName, behavior) = actor;
      startActor(control, actorName, behavior) :: acc)
      (solve startPolicy);
    control

///
/// Starts a single actor. Returns its reference.
///
pub def startActor(control: ControlChannel, actorName: Str, behavior: Mailbox -> ActorResult[Str]): ActorRef =
    let mailbox = chan ActorMsg 10;
    spawn (
      match behavior(mailbox) with {
        case Done =>
          // TODO: use the mailbox as the actor's identity, rather than a name? This would ensure uniqueness (whereas if we compose two actor systems, name clashes are bound to happen)
          control <- Terminated(actorName)
        case Crash(_) =>
          control <- Crashed(actorName)
      }
    );
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    // TODO: what happens when the actor crashes? This should be propagated to the master control channel.
    mailbox

pub def stop(_a: ActorSystem[_, _, _, _, _]): Unit =
    // TODO: Not unlike start, but also needs some running description of the actor system.
    ()

pub def react(_a: ActorSystem[_, _, _, _, _]): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()


// TODO: Does what we are doing work with scoped threads?

///////////////////////////////////////////////////////////////////////////////
/// Examples                                                                 //
///////////////////////////////////////////////////////////////////////////////

namespace CounterActorSystem {

    pub def main(): Unit =
        let _control = start(system());
        ()

    pub def system(): ActorSystem = {
            actors = #{
                ActorDef("counterActor", counterActor(0i32)).
                ActorDef("Worker1", workerActor(m)) :- MailboxOf("counterActor", m).
                ActorDef("Worker2", workerActor(m)) :- MailboxOf("counterActor", m).
                ActorDef("Worker3", workerActor(m)) :- MailboxOf("counterActor", m).
            } | defaultActorSystem() }

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready("counterActor"); counterActor(n, mailbox)
            case Stop           => Done
            case Msg(c, "GET")  => c <- Msg(mailbox, "<here we should send n>"); counterActor(n, mailbox) // TODO: Need a way to send the message.
            case Msg(_, "INC")  => counterActor(n + 1, mailbox)
            case Msg(_, "DEC")  => counterActor(n - 1, mailbox)
            case Msg(_, m)      => Crash("Unexpected message: " + m)
        }

    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Mailbox, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)           => c <- Ready("workerActor"); workerActor(counterActor, mailbox)
            case Stop               => Done
            case Msg(_, "wakeup")   => counterActor <- Msg(mailbox, "INC"); workerActor(counterActor, mailbox)
            case Msg(_, m)          => Crash("Unexpected message: " + m)
        }

}

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

// Properties of actors:
// - FIFO / First that match
// - Extending actors
