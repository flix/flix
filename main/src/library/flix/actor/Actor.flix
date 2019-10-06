// TODO: Currently this file crashes the flix compiler due to the issues:

// TODO [Synthesize]: Ensure that there is proper equality on records.
// TODO [Synthesize]: Ensure that there is proper equality on channels.


// TODO: Define core data types here.
// TODO: Carefully document each type and case.
// -----------------------------------------------------------------------------------------------

pub enum ActorResult[a] {
    case Stopped(a),
    case Crashed(a)
}

pub enum ActorMsg[a] {
    case Start,
    case Stop,
    case Msg(a)
}

pub enum ActorSystem {
    // TODO: Use record here or just a tuple?
    case ActorSystem({startPolicy: #{Actor}, shutdownPolicy: #{Actor}, restartPolicy: #{Actor}})
}

// TODO: Define core relations here:
// -----------------------------------------------------------------------------------------------
pub rel Actor[s](actorName: Str, initialState: s, mailbox: Channel[ActorMsg[s]], actor: s -> ActorResult[s])
pub rel Started(actorName: Str)
pub rel Stopped(actorName: Str)
pub rel Crashed(actorName: Str)

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

pub def startActor[s](actorName: Str, initialState: s, mailbox: Channel[ActorMsg[s]], actor: s -> ActorResult[s]): Unit =
    // TODO: Spawn the actor using the initial state.
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    ()

pub def stop(a: ActorSystem): Unit =
    // TODO: Not unlike start, but also needs some running description of the actor system.
    ()

pub def react(a: ActorSystem): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()



// TODO: Add examples below here:
// -----------------------------------------------------------------------------------------------
namespace CounterActorSystem{

    def counterActor(n: Int, m: Channel[ActorMsg[Int]]): ActorResult[Int] =
        match <- m {
            case Start => ??? // TODO: Need to reply that we have started.
            case Stop => ???
            case Msg(_) => ???
        }

}





pub def example02(): Unit =
    ()