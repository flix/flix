///
/// An actor result represents the outcome of an actor.
///
/// An actor must (eventually) return with one of three values:
///
/// - An actor must return `Done` when it has successfully completed its work.
/// - An actor must return `Crash(k, e)` when it encounters an error `e` it cannot handle normally,
///   but it still restartable using the continuation `k`.
///   The continuation `k` should encapsulate the last known good state of the actor.
/// - An actor must return `Fatal(e)` when it encounters an error `e` and cannot be restarted.
///   The error is permanent and there is no hope of restarting the actor from its current state.
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
    /// @param k the continuation that can be used to restart the actor.
    /// @param e the error value.
    ///
    case Crash(Actor, e), // TODO: Rename to ResumableError?

    ///
    /// Indicates that the actor crashed with error value `e` and cannot be restarted.
    ///
    /// @param e the error value.
    ///
    case FatalCrash(e)    // TODO: Rename to NonResumableError?
}
