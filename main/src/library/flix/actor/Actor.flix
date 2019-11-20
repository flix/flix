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



/// An actor receives messages of type ActorMsg.
/// These can either be a message indicating to start (`Start`) or stop (`Stop`) the actor, or an actuall application-level message (`Msg`).
/// Upon reception of a `Start` message, the actor has to reply on the given channel with a `Ready` message.
/// Upon reception of `Stop` message, the actor has to ends its execution. TODO: does it? If not, what happens?
/// When receiving a `Msg` message, the actor can reply to the sender of the message using the argument channel.

// TODO: Do we want two types of messages depending on whether we need a reply channel?
// Q: I would not have two types of messages, otherwise it is not clear from the sender what to send (imagine an actor that uses the control channel and then changes its behaviour to not using the control channel)
pub enum ActorMsg {
    case Start(Channel[Ready]),
    case Stop,
    case Msg(Mailbox, Str)
}

///
/// A message sent to indicate that an actor is ready to receive messages.
///
pub enum Ready {
    case Ready
}

///
/// An actor, while running, communicates with the actor system by sending control messages.
///
/// An actor must send the `Ready` message when it has started and is ready to receive messages.
///
// TODO: These are internal events to the actor system.
pub enum ActorEvent {
    ///
    /// Indicates that the actor has started and is ready to receive messages.
    ///
    case ActorReady(Str),

    ///
    /// Indicates that an actor has crashed.
    ///
    case ActorCrashed(Str),

    ///
    /// Indicates that an actor has terminated.
    ///
    case ActorTerminated(Str)
}

///
/// A mailbox is a type alias for a channel of actor messages.
///
type alias Mailbox = Channel[ActorMsg]

///
/// An actor is identified by its mailbox
///
type alias ActorRef = Mailbox

// TODO: Introduce an use the definition of Actor in the definition of Actor.


///
/// A type alias for the row type of an actor policy.
///
type alias ActorPolicy = #{Start, Stop, ActorDef, MailboxOf, ActorState, DependsOn, WaitingOnDependency, DependsOnTrans} // TODO: sort by some sane order

// TODO: List of auxilliary predicates. It would be nice if these could be better "hidden":
rel WaitingOnDependency(actor: Str)
rel DependsOnTrans(actor1: Str, actor2: Str)




///
/// Returns the default start policy.
///
pub def defaultStartPolicy(): ActorPolicy = #{
    // Compute the actors that are waiting for a dependency to start.
    WaitingOnDependency(actor1) :- DependsOn(actor1, actor2), ActorState(actor2, Pending).

    // Start all actors whose dependencies are already running.
    Start(actor) :- ActorDef(actor, _), not WaitingOnDependency(actor).

    // Compute the transitive closure of the dependency relation.
    DependsOnTrans(x, y) :- DependsOn(x, y).
    DependsOnTrans(x, z) :- DependsOnTrans(x, y), DependsOn(y, z).

    // Start actors that have a cyclic dependency.
    Start(actor) :- ActorDef(actor, _), DependsOnTrans(actor, actor).

    // TODO: We actually want to do something better. We want to start all actors on a cycle, but only if their deps. have started.
}

///
/// Returns a policy that immediately starts all actors regardless of their dependencies.
///
pub def immediatelyStartAllPolicy(): ActorPolicy = #{
    Start(actor) :- ActorDef(actor, _).
}

///
/// Returns the default shutdown policy.
///
pub def defaultShutdownPolicy(): ActorPolicy = #{
    // TODO: incorporate DependsOn and deal with cyclic dependencies.
    Stop(actor) :- ActorState(actor, Running).
}

///
/// Returns a policy that immediately shuts down all actors regardless of their dependencies.
///
pub def immediatelyShutdownAllPolicy(): ActorPolicy = #{
    Stop(actor) :- ActorState(actor, Running).
}

///
/// Returns the default restart policy.
///
pub def defaultRestartPolicy(): ActorPolicy = oneForOneRestartPolicy()

///
/// Returns a restart policy that restarts an actor when it crashes.
///
pub def oneForOneRestartPolicy(): ActorPolicy = #{
    Start(actor) :- ActorState(actor, Crashed).
}

///
/// Returns a restart policy that restarts an actor along with its siblings when it crashes.
///
pub def oneForAllRestartPolicy(): ActorPolicy = #{
    // TODO: Need some notion of restart here? A stop followed by a start... (?)
}

///
/// An actor is defined by its behavior, which is a function from a mailbox to an actor result.
///
type alias Actor = Mailbox -> ActorResult[Str]

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

///
/// A relation that represents a partial order of actor dependencies.
///
/// If an actor `a1` depends on an actor `a2` then `a1` is started before `a2`.
/// If there is a cyclic dependency then both actors are started at the same time.
///
pub rel DependsOn(actor1: Str, actor2: Str)

///
/// An actor in the actor system is in one of the following statesÆ
///
/// - Pending: the actor has been declared, but never started.
/// - Running: the actor is running and processing messages.
/// - Stopped: the actor is gracefully stopped.
/// - Crashed: the actor has crashed.
///
pub enum State {
    case Pending,
    case Running,
    case Stopped,
    case Crashed
}


///
/// A predicate that describes the current state of an actor.
///
/// @param actorName the name of the actor.
/// @param state     the state of the actor. (An actor has exactly one state).
///
pub rel ActorState(actorName: Str, state: State)


pub rel Start(actorName: Str)
pub rel Stop(actorName: Str)
// TODO: Add resume?
// TODO: Use an enum instead and call it ActorAction?

pub rel MailboxOf(actorName: Str, mailbox: Mailbox) // TODO also just call this Mailbox or maybe use the words "Mailbox" and "Inbox" ?
// TODO: how to populate this relation? Should be done by the actor system somehow...

pub type alias RunningActorSystem = {control: Channel[ActorEvent], actors: List[(Str, ActorRef)], system: ActorSystem}


    pub def getActor(sys: RunningActorSystem, actorName: Str): Option[ActorRef] =
        Option.map((pair -> let (_, ref) = pair; ref),
            List.find((pair -> let (name, _) = pair; name == actorName), sys.actors))


///
/// Starts a single actor. Returns its reference.
///
pub def startActor(control: Channel[ActorEvent], actorName: Str, behavior: Actor): ActorRef =
    let mailbox = chan ActorMsg 10;
    spawn (
      match behavior(mailbox) with {
        case Done =>
          // TODO: use the mailbox as the actor's identity, rather than a name? This would ensure uniqueness (whereas if we compose two actor systems, name clashes are bound to happen)
          control <- ActorTerminated(actorName)
        case Crash(_) =>
          control <- ActorCrashed(actorName)
      }
    );
    // TODO: Use a channel to wait for termination.
    // TODO: Somehow communicate this information somewhere, if something happens? Maybe we need one giant control channel?
    // TODO: what happens when the actor crashes? This should be propagated to the master control channel.
    mailbox



pub def react(_system: RunningActorSystem): Unit =
    // TODO: function executed in response to changes in actor state. Should invoke the restart policy.
    ()

///////////////////////////////////////////////////////////////////////////////
/// Examples                                                                 //
///////////////////////////////////////////////////////////////////////////////

namespace CounterActorSystem {

    ///
    /// The main entry point for the counter actor system.
    ///
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        match getActor(sys, "counterActor") {
            case Some(master) =>
                let replyChan = chan ActorMsg 10;
                master <- Msg(replyChan, "INC");
                master <- Msg(replyChan, "INC");
                master <- Msg(replyChan, "GET");
                match <- replyChan {
                    case Msg(_, msg) => Console.printLine(msg)
                    case _ => Console.printLine("unexpected message")
                }
            case None => Console.printLine("did not find counterActor")
        }
      // shutdown(sys)


    ///
    /// Returns an actor system with one counter actor and several worker actors.
    ///
    pub def system(): ActorSystem = {
            actors = #{
                ActorDef("counterActor", counterActor(0i32)).
                ActorDef("worker1", workerActor(m)) :- MailboxOf("counterActor", m).
                ActorDef("worker2", workerActor(m)) :- MailboxOf("counterActor", m).
                ActorDef("worker3", workerActor(m)) :- MailboxOf("counterActor", m).
            } | ActorSystem.default() }

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready; counterActor(n, mailbox)
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
            case Start(c)           => c <- Ready(); workerActor(counterActor, mailbox)
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
