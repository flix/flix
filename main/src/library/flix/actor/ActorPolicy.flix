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
    Start(x) :- ActorDef(x, _), not Blocked(x).
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

