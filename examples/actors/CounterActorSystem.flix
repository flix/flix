namespace CounterActorSystem {
    ///
    /// The main entry point for the counter actor system.
    ///
    // TODO: Enable @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        let replyChan = chan Message 10;
        match <- replyChan {
            case Msg(msg, _) => Console.printLine(msg)
            case _ => Console.printLine("unexpected message")
        };
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and several worker actors.
    ///
    pub def system(): ActorSystem = {
            actors = Map#{
                "counterActor" -> counterActor(0i32),
                "worker1" -> workerActor(None),
                "worker2" -> workerActor(None),
                "worker3" -> workerActor(None)
            } | ActorSystem.default() }

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready; counterActor(n, dir, mailbox)
            case Stop           => Done
            case Msg("GET", c)  =>
                // TODO: Need a way to send the message.
                c <- Msg("<here we should send n>", mailbox);
                counterActor(n, dir, mailbox)
            case Msg("INC", _)  =>
                if (n < 2147483647)
                    counterActor(n + 1, dir, mailbox)
                else
                    NonResumableCrash("Counter overflowed.")
            case Msg("DEC", _)  => counterActor(n - 1, dir, mailbox)
            case Msg(m, _)      => ResumableCrash(counterActor(n), "Unexpected message: " + m)
        }

    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Option[Mailbox], dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)           =>
              let counter = ActorDirectory.getActor(dir, "counterActor", Running);
              c <- Ready();
              workerActor(counter, dir, mailbox)
            case Stop               => Done
            case Msg("wakeup", _)   =>
                Option.map(c -> c <- Msg("INC", mailbox), counterActor);
                workerActor(counterActor, dir, mailbox)
            case Msg(m, _)          => ResumableCrash(workerActor(counterActor), "Unexpected message: " + m)
        }
}
