///
/// An actor system with a master-worker model.
///
/// If a worker crashes once it is restarted. If it crashes again, the parent and all children are restarted.
///
namespace RestartOnce {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(1000i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and multiple worker actors.
    ///
    /// The actor system uses default policies.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "master" -> masterActor,
            "worker1" -> workerActor,
            "worker2" -> workerActor,
            "worker3" -> workerActor
        };
        let restartPolicy = #{
            // Compute the actors that have non-resumably crashed in the last second.
            Blocked(x) :-
                Log(x, Running, NonResumablyCrashed, timestamp1),
                Log(x, Running, NonResumablyCrashed, timestamp2),
                if timestamp1 != timestamp2.

            Start(x) :- ActorState(x, NonResumablyCrashed), not Blocked(x).


        };
        { actors = actors, restartPolicy = restartPolicy | default }
    }

    ///
    /// A master actor that communicates with its workers.
    ///
    pub def masterActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("Master Ready...");
                mailbox <- Msg("GO", mailbox);
                mailbox <- Msg("GO", mailbox);
                mailbox <- Msg("GO", mailbox);
                mailbox <- Msg("GO", mailbox);
                mailbox <- Msg("GO", mailbox);
                c <- Ready;
                masterActor(dir, mailbox)
            case Stop => Done
            case Msg("GO", _) =>
                let _ = {
                    let flatMap = Option.flatMap;
                    let* worker1 = ActorDirectory.getActor(dir, "worker1", Running);
                    let* worker2 = ActorDirectory.getActor(dir, "worker2", Running);
                    let* worker3 = ActorDirectory.getActor(dir, "worker3", Running);
                    worker1 <- Msg("WORK", mailbox);
                    worker2 <- Msg("KABOOM", mailbox);
                    worker3 <- Msg("WORK", mailbox);
                    None
                };
                masterActor(dir, mailbox)
            case Msg(msg, _)    => ResumableCrash(masterActor, "Unexpected message: " + msg)
        }

    ///
    /// An worker actor that occasionally explodes.
    ///
    pub def workerActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("Worker Ready...");
                c <- Ready;
                workerActor(dir, mailbox)
            case Stop => Done
            case Msg("WORK", _) =>
                Console.printLine("Working!");
                workerActor(dir, mailbox)
            case Msg("KABOOM", _) =>
                Console.printLine("Kaboom!");
                NonResumableCrash("WHAM!")
            case Msg(msg, _)    => ResumableCrash(workerActor, "Unexpected message: " + msg)
        }

}
