///
/// An actor system with a master-worker model.
///
/// If a worker actor crashes *once* it is restarted. If it crashes again, everything is stopped.
///
namespace RestartAndStop {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(500i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and multiple worker actors.
    ///
    /// The actor system uses default policies.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "master" -> masterActor(Nil),
            "worker1" -> workerActor,
            "worker2" -> workerActor,
            "worker3" -> workerActor
        };
        let restartPolicy = #{
            // Compute the actors that have non-resumably crashed more than once.
            Blocked(x) :-
                Log(x, Running, NonResumablyCrashed, timestamp1),
                Log(x, Running, NonResumablyCrashed, timestamp2),
                if timestamp1 != timestamp2.

            Start(x) :- ActorState(x, NonResumablyCrashed), not Blocked(x).

            // TODO: Magnus It is becoming painful to have to reuse "Blocked".
            // TODO: Would be better to have a name "Workers".
            Stop(x) :- Actor(x), Blocked("worker1").
            Stop(x) :- Actor(x), Blocked("worker2").
            Stop(x) :- Actor(x), Blocked("worker3").

        };
        { actors = actors, restartPolicy = restartPolicy | default }
    }

    ///
    /// A master actor that communicates with its workers.
    ///
    pub def masterActor(workers: List[Mailbox], dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("[Master] Ready");
                mailbox <- Msg("GO", mailbox);
                let workers = Option.getWithDefault({
                    let flatMap = Option.flatMap;
                    let* worker1 = ActorDirectory.getActor(dir, "worker1", Running);
                    let* worker2 = ActorDirectory.getActor(dir, "worker2", Running);
                    let* worker3 = ActorDirectory.getActor(dir, "worker3", Running);
                    Some(worker1 :: worker2 :: worker3 :: Nil)
                    }, Nil);
                c <- Ready;
                masterActor(workers, dir, mailbox)
            case Stop =>
                Console.printLine("[Master] Stopped.");
                Done
            case Msg("GO", _) =>
                workers |> List.mapWithIndex((worker, index) -> {
                    if (index == 0) {
                        worker <- Msg("KABOOM", mailbox)
                    } else {
                        worker <- Msg("WORK", mailbox)
                    }
                });
                <- Timer.milliseconds(50i64);
                mailbox <- Msg("GO", mailbox);
                masterActor(workers, dir, mailbox)
            case Msg(msg, _)    => ResumableCrash(masterActor(workers), "Unexpected message: " + msg)
        }

    ///
    /// An worker actor that can work or explode.
    ///
    pub def workerActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("[Worker] Ready.");
                c <- Ready;
                workerActor(dir, mailbox)
            case Stop =>
                Console.printLine("[Worker] Stopped.");
                Done
            case Msg("WORK", _) =>
                Console.printLine("[Worker] Working.");
                workerActor(dir, mailbox)
            case Msg("KABOOM", _) =>
                Console.printLine("[Worker] Kaboom!");
                NonResumableCrash("WHAM!")
            case Msg(msg, _)    => ResumableCrash(workerActor, "Unexpected message: " + msg)
        }

}
