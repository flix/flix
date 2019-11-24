///
/// An actor in the actor system is in one of the following states:
///
/// - Pending:              the actor is waiting for dependencies before it can start.
/// - Running:              the actor is running and processing messages.
/// - Stopped:              the actor is gracefully stopped. The actor may be started again with its initial state.
/// - ResumablyCrashed:     the actor has crashed, but may be resumed.
/// - NonResumablyCrashed:  the actor has crashed and cannot be resumed.
///
pub enum State {
    case Pending,
    case Running,
    case Stopped,
    case ResumablyCrashed,
    case NonResumablyCrashed
}
