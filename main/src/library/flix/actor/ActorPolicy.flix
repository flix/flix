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

