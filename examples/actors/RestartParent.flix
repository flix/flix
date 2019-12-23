///
/// An actor system with a master-worker model where a crash in a worker restarts the worker and master.
///
namespace RestartParent {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(50i64);
        ActorDirectory.send(sys, "worker2", Msg("OHNO", Channel.new()));
        <- Timer.milliseconds(50i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns the actor system.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "master" -> masterActor,
            "worker1" -> workerActor,
            "worker2" -> workerActor,
            "worker3" -> workerActor
        };
        let dependencies = #{
            DependsOn("worker1", "master").
            DependsOn("worker2", "master").
            DependsOn("worker3", "master").
        };
        let restartPolicy = #{
            // TODO: Magnus: It would be useful to have a predicate to group the workers.

            // Start a non-resumably crashed actor.
            Start(x) :- ActorState(x, NonResumablyCrashed).

            // Restart the master, if one of its workers crashes.
            Restart("master") :- ActorState("worker1", NonResumablyCrashed).
            Restart("master") :- ActorState("worker2", NonResumablyCrashed).
            Restart("master") :- ActorState("worker3", NonResumablyCrashed).
        };
        { actors = actors,
          startPolicy = defaultStartPolicy() <+> dependencies,
          shutdownPolicy = defaultShutdownPolicy() <+> dependencies,
          restartPolicy = restartPolicy
          | default }
    }

    ///
    /// A master actor.
    ///
    pub def masterActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[Master] Ready."); c <- Ready; masterActor(dir, mailbox)
            case Stop           => Console.printLine("[Master] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(masterActor, "Unexpected message: " + msg)
        }

    ///
    /// A worker actor.
    ///
    pub def workerActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[Worker] Ready."); c <- Ready; workerActor(dir, mailbox)
            case Stop           => Console.printLine("[Worker] Stopped."); Done
            case Msg("OHNO", _) => Console.printLine("[Worker] Crashed."); NonResumableCrash("WHAM!")
            case Msg(msg, _)    => ResumableCrash(workerActor, "Unexpected message: " + msg)
        }

}
