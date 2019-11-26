///
/// An actor in the actor system is in one of the following states:
///
/// - InTransition:         the actor is in the process of starting or stopping. Its state cannot be changed before it either sends `Ready` or returns an actor result.
/// - Running:              the actor is running and processing messages.
/// - Stopped:              the actor is gracefully stopped. The actor may be started again with its initial state.
/// - ResumablyCrashed:     the actor has crashed, but may be resumed.
/// - NonResumablyCrashed:  the actor has crashed and cannot be resumed.
///
pub enum State {
    case InTransition, // TODO: Rename to transient?
    case Running,
    case Stopped,
    case ResumablyCrashed,
    case NonResumablyCrashed
}
