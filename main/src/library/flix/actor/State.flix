///
/// An actor in the actor system is in one of the following states:
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
    case Crashed,
    case FatallyCrashed
}

///
/// A predicate that describes the current state of an actor.
///
/// @param name     the name of the actor.
/// @param state    the state of the actor. An actor has exactly one state.
///
pub rel ActorState(name: Str, state: State)
