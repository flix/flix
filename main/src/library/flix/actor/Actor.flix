// TODO: Independent Tasks:

// TODO: Add pattern matching on records. (Magnus has a student that may get this done).
// TODO: Add equality on records (if we want to use them in facts).

// TODO: Add equality on closures.



///////////////////////////////////////////////////////////////////////////////
/// Actor System Implementation                                              //
///////////////////////////////////////////////////////////////////////////////



// TODO: Currently not polymorphic to keep it simple.

pub enum ActorResult {
    case Done,
    case Crash(Str) // TODO: Should probably crash with both a message and the current state?
}

pub enum ActorMsg {
    case Start(Channel[Ready]),
    case Stop,
    case Msg(Str)
}

pub enum Ready {
    case Ready
}

///
/// An actor system is essentially a triple that consists of:
///
/// A `startPolicy` that describes the actors in the system and how to start them.
/// A `shutdownPolicy` that describes how to shutdown the actor system in an orderly fashion.
/// A `restartPolicy` that describes what should happen when an actor crashes.
///
pub enum ActorSystem {
    case ActorSystem({startPolicy: #{Actor}, shutdownPolicy: #{Actor}, restartPolicy: #{Actor}})
}

// TODO: Define core relations here:
// -----------------------------------------------------------------------------------------------
pub rel Actor[s](actorName: Str, initialState: s, mailbox: Channel[ActorMsg], actor: s -> ActorResult)

pub rel Started(actorName: Str)
pub rel Stopped(actorName: Str)
pub rel Crashed(actorName: Str)
pub rel Start(actorName: Str)

// TODO: We dont really have proper support for type aliases, we might want to fix that.

// TODO: Define core functions here
// -----------------------------------------------------------------------------------------------

pub def start(a: ActorSystem): Unit =
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

pub def startActor[s](_actorName: Str, _initialState: s, _mailbox: Channel[ActorMsg], _actor: s -> ActorResult): Unit =
    // TODO: Spawn the actor using the initial state.
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    ()

pub def stop(_a: ActorSystem): Unit =
    // TODO: Not unlike start, but also needs some running description of the actor system.
    ()

pub def react(_a: ActorSystem): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()

///
/// Returns the default restart policy ...
///
pub def defaultRestartPolicy(): #{Actor} = ???

///
/// ...
///
pub def restartPolicy(): #{Start, Crashed} = #{
    Start(actorRef) :- Crashed(actorRef).
}



///////////////////////////////////////////////////////////////////////////////
/// Examples                                                                 //
///////////////////////////////////////////////////////////////////////////////

namespace CounterActorSystem{

    pub def main(): Unit =
        () // TODO: Start the actor system here.


    pub def system(): ActorSystem = ActorSystem({
        startPolicy = #{
            // TODO...
        },
        restartPolicy = #{
            // TODO...
        },
        shutdownPolicy = #{
            // TODO...
        }}
    )

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, mailbox: Channel[ActorMsg]): ActorResult =
        match <- mailbox {
            case Start(c)   => c <- Ready; counterActor(n, mailbox)
            case Stop       => Done
            case Msg("GET") => ??? // TODO: How can we reply, when we dont have the sender?
            case Msg("INC") => counterActor(n + 1, mailbox)
            case Msg("DEC") => counterActor(n - 1, mailbox)
            case Msg(m)     => Crash("Unexpected message: " + m)
        }

    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Channel[ActorMsg], mailbox: Channel[ActorMsg]): ActorResult =
        match <- mailbox {
            case Start(c)       => c <- Ready; workerActor(counterActor, mailbox)
            case Stop           => Done
            case Msg("wakeup")  => counterActor <- Msg("INC"); workerActor(counterActor, mailbox)
            case Msg(m)         => Crash("Unexpected message: " + m)
        }

}
