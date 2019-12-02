///
/// A type alias for the row type of an actor policy.
///
type alias ActorPolicy = #{Actor, ActorState, MailboxOf, DependsOn, Start, Stop, Resume, Restart, Log, Now, Blocked}

// TODO: Could we have the start policy only using the Start predicate. And the stop policy NOT using the start predicate.
// TODO: But we do need the join for the "restartPolicy" (which maybe we should rename to "runningPolicy")
// TODO: The whole actor system progresses through start -> running -> stop


///
/// A relation that represents actors in the system.
///
/// If two or more actor behaviors are attached to the same name then messages are load balanced between them.
///
pub rel Actor(name: Str, behavior: ActorBehavior)

///
/// A relation that maps actors to mailboxes.
///
pub rel MailboxOf(name: Str, mailbox: Mailbox)

///
/// A relation that represents the dependencies between actors.
///
/// If an actor `x` depends on an actor `y` then `x` is started before `y`.
///
/// If there is a cyclic dependency then both actors are started at the same time.
///
pub rel DependsOn(name1: Str, name2: Str)

///
/// A relation used to specify the actors that should be started by an actor policy.
///
pub rel Start(name: Str)

///
/// A relation used to specify the actors that should be stopped by an actor policy.
///
pub rel Stop(name: Str)

///
/// A relation used to specify the actors that should be restarted (with a fresh state) by an actor policy.
pub rel Restart(name: Str)

///
/// A relation used to specify the actors that should be resume (from their last known good state) by an actor policy.
///
pub rel Resume(name: Str)

///
/// A relation that tracks past actor transitions.
///
pub rel Log(name: Str, last: State, next: State, timestamp: Instant)

///
/// A relation that holds the current time.
///
pub rel Now(timestamp: Instant)

///
/// If an actor policy specifies multiple actions for the same actor then the final action is computed by join:
///
/// Start, Start  => Start
/// Start, Stop   => Start
/// Stop, Start   => Start
/// Resume, Stop  => Resume
/// Stop, Resume  => Resume
/// Start, Resume => Resume
/// Resume, Start => Resume
/// x, x          => x
///

///
/// An action is combined with an actor state according to the following table:
///
/// <<no-state>:
///     - Start     => start the actor.
///     - Stop      => no-op.
///     - Resume    => no-op.
/// InTransition (actor is changing its state):
///     - Start     => no-op.
///     - Stop      => no-op.
///     - Resume    => no-op.
///  Running:
///     - Start     => no-op.
///     - Stop      => stop the actor.
///     - Resume    => no-op.
///  Stopped:
///     - Start     => start the actor.
///     - Stop      => no-op.
///     - Resume    => start the actor (with the initial state).
/// ResumablyCrashed:
///     - Start     => restart the actor with the initial state. (required to allow a reset of the actor)
///     - Stop      => change state to stopped (clears the error) (required to allow the error to be cleared).
///     - Resume    => resume.
/// NonResumablyCrashed:
///     - Start     => start the actor with initial state.
///     - Stop      => change state to stopped (clears the error).
///     - Resume    => start the actor with initial state.
///

///
/// A predicate that describes the current state of an actor.
///
/// @param name     the name of the actor.
/// @param state    the state of the actor. An actor has exactly one state.
///
pub rel ActorState(name: Str, state: State)

///
/// Auxiliary relation: An actor is blocked if one or more of its (transitive) dependencies is not yet running.
///
rel Blocked(actor: Str)

///
/// Returns the default start policy.
///
pub def defaultStartPolicy(): ActorPolicy = #{
    // Compute the transitive closure of the DependsOn relation.
    DependsOn(x, z) :- DependsOn(x, y), DependsOn(y, z).

    // Compute the actors that are waiting for one or more of its dependencies to be running.
    // Note1: An actor is *not* blocked if it depends on itself. This allows actors in cyclic dependencies to be started.
    // Note2: An actor that has no dependencies is trivially *never* blocked.
    Blocked(x) :- DependsOn(x, y), not ActorState(y, Running), not DependsOn(x, x).

    // Start actors that are not blocked.
    Start(x) :- Actor(x, _), not Blocked(x).
}

///
/// Returns a policy that immediately starts all actors regardless of their dependencies.
///
pub def immediatelyStartAllPolicy(): ActorPolicy = #{
    Start(actor) :- Actor(actor, _).
}

///
/// Returns the default shutdown policy.
///
pub def defaultShutdownPolicy(): ActorPolicy = #{
    // Compute the transitive closure of the DependsOn relation.
    DependsOn(x, z) :- DependsOn(x, y), DependsOn(y, z).

    // Compute the actors that are waiting for one or more if its dependencies to be stopped.
    // Note1: An actor is *not* blocked if it depends on itself. This allows actors in cyclic dependencies to be stopped.
    // Note2: An actor that no-one depends on is trivially *never* blocked.
    Blocked(x) :- DependsOn(x, y), ActorState(y, Running), not DependsOn(x, x).

    // Stop actors that are not blocked.
    Stop(x) :- Actor(x, _), ActorState(x, Running).
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
    Resume(x) :- ActorState(x, ResumablyCrashed).
}

///
/// Returns a restart policy that restarts an actor along with its siblings when it crashes.
///
pub def oneForAllRestartPolicy(): ActorPolicy = #{
    // TODO: Magnus: Implement this. Should it be based on depends on or some other relation?
}

// TODO: Magnus: Implement more policies.