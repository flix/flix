///
/// An actor result represents the outcome of an actor.
///
/// An actor must (eventually) return with one of three values:
///
/// - An actor must return `Done` when it has successfully completed its work.
///
/// - An actor must return `ResumableCrash(k, e)` when it encounters an error `e` it cannot handle normally,
///   but the actor can still be resumed with the continuation `k`. The continuation `k` should
///   encapsulate the last known good state of the actor.
///
/// - An actor must return `NonResumableCrash(e)` when it encounters an error `e` and cannot be resumed.
///   The error is permanent and there is no hope of resuming the actor from its current state.
///
/// @param e the type of the error.
///
pub enum ActorResult[e] {
    ///
    /// Indicates that the actor terminated normally.
    ///
    case Done,

    ///
    /// Indicates that the actor crashed with error value `e`.
    ///
    /// @param k the continuation that can be used to resume the actor.
    /// @param e the error value.
    ///
    case ResumableCrash(Actor, e),

    ///
    /// Indicates that the actor crashed with error value `e` and cannot be resumed.
    ///
    /// @param e the error value.
    ///
    case NonResumableCrash(e)
}
