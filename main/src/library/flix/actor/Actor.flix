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
pub enum ActorResult[s, e] {
    ///
    /// Indicates that the actor successfully completed its work.
    ///
    case Done,

    ///
    /// Indicates that the actor was unable to complete it works and aborted.
    ///
    case Crash(s, e)
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
    case Ready
}



pub enum ActorMsg {
    case Start(Channel[ControlMsg]),
    case Stop,
    case Msg(Channel[ActorMsg], Str)
}



///
/// An actor system is essentially a triple that consists of:
///
/// A `startPolicy` that describes the actors in the system and how to start them.
/// A `shutdownPolicy` that describes how to shutdown the actor system in an orderly fashion.
/// A `restartPolicy` that describes what should happen when an actor crashes.
///
pub enum ActorSystem[s, r1, r2, r3] {
    case ActorSystem({startPolicy: #{Actor[s] | r1}, shutdownPolicy: #{Actor[s] | r2}, restartPolicy: #{Actor[s] | r3}}) // TODO: The types here are a bit poor
}

// TODO: Define core relations here:
// -----------------------------------------------------------------------------------------------
pub rel Actor[s](actorName: Str, initialState: s, mailbox: Channel[ActorMsg], actor: (s, Channel[ActorMsg]) -> ActorResult[s, Str])

pub rel Started(actorName: Str)
pub rel Stopped(actorName: Str)
pub rel Crashed(actorName: Str)

pub rel Start(actorName: Str)

// TODO: We dont really have proper support for type aliases, we might want to fix that.

// TODO: Define core functions here
// -----------------------------------------------------------------------------------------------

pub def start(a: ActorSystem[s, r1, r2, r3]): Unit =
    // TODO: We lack pattern matching on records.
    let ActorSystem(r) = a;
    let _startPolicy = r.startPolicy;
    // TODO: 1. Compute the fixpoint of startPolicy
    // TODO: 2. Compute a fold over the Start relation (?)
    //          TODO: Requires support for fold in the language.
    //          TODO: A better idea might be a toSet primitive that converts a relation to a set of tuples?
    // TODO: 3. Start each actor.
    // TODO: 4. Return a value that can be used for shutdown.
    ()

pub def startActor[s, e](_actorName: Str, _initialState: s, _mailbox: Channel[ActorMsg], _actor: s -> ActorResult[s, e]): Unit =
    // TODO: Spawn the actor using the initial state.
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    ()

pub def stop(_a: ActorSystem[r1, r2, r3]): Unit =
    // TODO: Not unlike start, but also needs some running description of the actor system.
    ()

pub def react(_a: ActorSystem[r1, r2, r3]): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()

///
/// Returns the default restart policy.
///
pub def defaultRestartPolicy[r](): #{Start, Crashed | r} = #{
    Start(actorRef) :- Crashed(actorRef).
}

// TODO: Does what we are doing work with scoped threads?

///////////////////////////////////////////////////////////////////////////////
/// Examples                                                                 //
///////////////////////////////////////////////////////////////////////////////

namespace CounterActorSystem{

    pub def main(): Unit =
        () // TODO: Start the actor system here.


    pub def system(): ActorSystem[_, _, _, _] = ActorSystem({
        startPolicy = #{
            Start("counterActor").

            Start("Worker1") :- Started("counterActor").
            Start("Worker2") :- Started("counterActor").
            Start("Worker3") :- Started("counterActor").

// TODO: Problem with the type system...
//            Actor("counterActor", 0i32, chan ActorMsg 10, counterActor).

        },
        restartPolicy = defaultRestartPolicy(),
        shutdownPolicy = #{
            // TODO...
        }}
    )

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, mailbox: Channel[ActorMsg]): ActorResult[Int, Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready; counterActor(n, mailbox)
            case Stop           => Done
            case Msg(c, "GET")  => c <- Msg(mailbox, "<here we should send n>"); counterActor(n, mailbox) // TODO: Need a way to send the message.
            case Msg(_, "INC")  => counterActor(n + 1, mailbox)
            case Msg(_, "DEC")  => counterActor(n - 1, mailbox)
            case Msg(_, m)      => Crash(n, "Unexpected message: " + m)
        }

    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Channel[ActorMsg], mailbox: Channel[ActorMsg]): ActorResult[Unit, Str] =
        match <- mailbox {
            case Start(c)           => c <- Ready; workerActor(counterActor, mailbox)
            case Stop               => Done
            case Msg(_, "wakeup")   => counterActor <- Msg(mailbox, "INC"); workerActor(counterActor, mailbox)
            case Msg(_, m)          => Crash((), "Unexpected message: " + m)
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
/// - Add support for fold.
///
