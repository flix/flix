///
/// A type alias for the row type of an actor policy.
///
type alias ActorPolicy = #{ActorDef, ActorState, MailboxOf, DependsOn, Start, Stop, Resume, Blocked}

/// TODO: DOC And better name.
pub rel ActorDef(name: Str, actor: Actor)

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
/// A relation used to specify the actors that should be resume (from their last known good state) by an actor policy.
///
pub rel Resume(name: Str)

///
/// If an actor policy specifies multiple actions for the same actor then the final action is computed by join:
///
/// Start, Start  => Start
/// Start, Stop   => Stop
/// Stop, Start   => Stop
/// Resume, Stop  => Stop
/// Stop, Resume  => Stop
/// Start, Resume => Resume
/// Resume, Start => Resume
///

///
/// An action is combined with an actor state according to the following table:
///
/// <<no-state>:
///     - Start     => start the actor.
///     - Stop      => no-op.
///     - Resume    => start the actor.
/// Pending:
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
///     - Resume    => start the actor (with initial state).
/// ResumablyCrashed:
///     - Start     => start the actor with initial state.
///     - Stop      => change state to stopped (clears the error).
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
    Blocked(x) :- DependsOn(x, y), ActorState(y, Pending), not DependsOn(x, x).

    // Start actors that are not blocked.
    Start(x) :- ActorDef(x, _), ActorState(x, Pending), not Blocked(x).
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
    // Compute the transitive closure of the DependsOn relation.
    DependsOn(x, z) :- DependsOn(x, y), DependsOn(y, z).

    // Compute the actors that are waiting for one or more if its dependencies to be stopped.
    // Note1: An actor is *not* blocked if it depends on itself. This allows actors in cyclic dependencies to be stopped.
    // Note2: An actor that no-one depends on is trivially *never* blocked.
    Blocked(x) :- DependsOn(x, y), ActorState(y, Running), not DependsOn(x, x).

    // Stop actors that are not blocked.
    Stop(x) :- ActorDef(x, _), ActorState(x, Running).
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
    // TODO: Need some notion of restart here? A stop followed by a start... (?)
}

