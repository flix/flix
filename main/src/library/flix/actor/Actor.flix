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

// TODO: Introduce an use the definition of Actor in the definition of Actor.

///
/// An actor system is essentially a triple that consists of:
///
/// A `startPolicy` that describes the actors in the system and how to start them.
/// A `shutdownPolicy` that describes how to shutdown the actor system in an orderly fashion.
/// A `restartPolicy` that describes what should happen when an actor crashes.
///
pub enum ActorSystem[s, r1, r2, r3, r4] {
    case ActorSystem({actors: #{Actor[s] | r1}, startPolicy: #{Start | r2}, shutdownPolicy: #{Stop | r3}, restartPolicy: #{Start, Crashed | r4}})
}

// TODO: Define core relations here:
// -----------------------------------------------------------------------------------------------

/// An actor has:
///   - `actorName`: a name identifying the actor
///   - `initialState`: the initial state of the actor, used when (re)starting the actor
///   - `mailbox`: a channel where the actor receives messages from
///   - `actor`: the behavior of the actor as a function that takes the current state and mailbox, and returns when the actor is done or has crashed
// TODO: Rename to ActorDef or similar?
pub rel Actor[s](actorName: Str, initialState: s, mailbox: Mailbox, actor: (s, Mailbox) -> ActorResult[s, Str])

// TODO: Mailboxes should disappear from the Actor relation (which also needs a new name).

pub rel Started(actorName: Str)
pub rel Stopped(actorName: Str)
pub rel Crashed(actorName: Str)

pub rel Start(actorName: Str)
pub rel Stop(actorName: Str)

// TODO: We dont really have proper support for type aliases, we might want to fix that.

// TODO: Define core functions here
// -----------------------------------------------------------------------------------------------

pub def start[s, r1, r2, r3, r4](a: ActorSystem[s, r1, r2, r3, r4]): Unit /* Set[Channel[ActorMsg]] */ =
    // TODO: We lack pattern matching on records.
    let ActorSystem(r) = a;
    let startPolicy = r.startPolicy <+> r.actors;
    fold Actor () ((_actor, _acc) ->
      // TODO: This is what should happen, but this does not compile (init of empty list)
      // let (actorName, initialState, mailbox, behavior) = actor;
      // let channel = startActor(actorName, initialState, mailbox, behavior));
      // Set.insert(channel, acc)
      ())
      (solve startPolicy)
    // TODO: Return a value that can be used for shutdown.
    //       That value could be all actor's mailboxes (we can send a stop message on all of them at shutdown)

pub def startActor[s](_actorName: Str, initialState: s, mailbox: Mailbox, actor: (s, Mailbox) -> ActorResult[s, Str]): Mailbox =
    spawn (actor(initialState, mailbox));
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    mailbox

pub def stop(_a: ActorSystem[_, _, _, _, _]): Unit =
    // TODO: Not unlike start, but also needs some running description of the actor system.
    ()

pub def react(_a: ActorSystem[_, _, _, _, _]): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()

///
/// Returns the default restart policy.
/// This policy restarts any actor that has crashed.
///
pub def defaultRestartPolicy[r](): #{Start, Crashed | r} = #{
    Start(actorRef) :- Crashed(actorRef).
}

///
/// Returns the default start policy.
/// This policy starts all actors.
///
pub def defaultStartPolicy[s, r](): #{Start, Actor[s] | r} = #{
    Start(actorRef) :- Actor(actorRef, _, _, _).
}
// TODO: Does what we are doing work with scoped threads?

///////////////////////////////////////////////////////////////////////////////
/// Examples                                                                 //
///////////////////////////////////////////////////////////////////////////////

// namespace CounterActorSystem{
// 
//     pub def main(): Unit =
//         () // start(system()) // TODO: Start the actor system here.
// 
// 
//     pub def system(): ActorSystem[_, _, _, _] = ActorSystem({
//         actors = #{
//             Actor("counterActor", 0i32, chan ActorMsg 10, counterActor).
//             Actor("Worker1", c, chan ActorMsg 10, workerActor) :- Actor("counterActor", _, c, _).
//             Actor("Worker2", c, chan ActorMsg 10, workerActor) :- Actor("counterActor", _, c, _).
//             Actor("Worker3", c, chan ActorMsg 10, workerActor) :- Actor("counterActor", _, c, _).
//         },
//         startPolicy = #{
//             Start("counterActor").
// 
//             Start("Worker1") :- Started("counterActor").
//             Start("Worker2") :- Started("counterActor").
//             Start("Worker3") :- Started("counterActor").
//         },
//         restartPolicy = defaultRestartPolicy(),
//         shutdownPolicy = #{
//             Stop("counterActor").
//             Stop("Worker1"). Stop("Worker2"). Stop("Worker3").
//         }}
//     )
// 
//     ///
//     /// An actor that maintains an internal counter.
//     ///
//     /// The counter can be incremented, decrement, and its value retrieved.
//     ///
//     pub def counterActor(n: Int, mailbox: Mailbox: ActorResult[Int, Str] =
//         match <- mailbox {
//             case Start(c)       => c <- Ready; counterActor(n, mailbox)
//             case Stop           => Done
//             case Msg(c, "GET")  => c <- Msg(mailbox, "<here we should send n>"); counterActor(n, mailbox) // TODO: Need a way to send the message.
//             case Msg(_, "INC")  => counterActor(n + 1, mailbox)
//             case Msg(_, "DEC")  => counterActor(n - 1, mailbox)
//             case Msg(_, m)      => Crash(n, "Unexpected message: " + m)
//         }
// 
//     ///
//     /// An actor that occasionally requests the counter actor to increment its number.
//     ///
//     pub def workerActor(counterActor: Mailbox, mailbox: Mailbox): ActorResult[Unit, Str] =
//         match <- mailbox {
//             case Start(c)           => c <- Ready; workerActor(counterActor, mailbox)
//             case Stop               => Done
//             case Msg(_, "wakeup")   => counterActor <- Msg(mailbox, "INC"); workerActor(counterActor, mailbox)
//             case Msg(_, m)          => Crash((), "Unexpected message: " + m)
//         }
// 
// }

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

// Properties of actors:
// - FIFO / First that match
// - Extending actors
