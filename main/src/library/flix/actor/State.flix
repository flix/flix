///
/// An actor in the actor system is in one of the following states:
///
/// - Pending: the actor has been declared, but never started.
/// - Running: the actor is running and processing messages.
/// - Stopped: the actor is gracefully stopped.
/// - Crashed: the actor has crashed.
///
// TODO: Update naming and number of states.
pub enum State {
    case Pending,
    case Running,
    case Stopped,
    case Crashed,
    case FatallyCrashed
}

