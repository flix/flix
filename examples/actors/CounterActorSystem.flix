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
            actors = #{
                Actor("counterActor", counterActor(0i32)).
                Actor("worker1", workerActor(m)) :- MailboxOf("counterActor", m).
                Actor("worker2", workerActor(m)) :- MailboxOf("counterActor", m).
                Actor("worker3", workerActor(m)) :- MailboxOf("counterActor", m).
            } | ActorSystem.default() }

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    pub def counterActor(n: Int, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready; counterActor(n, mailbox)
            case Stop           => Done
            case Msg("GET", c)  => c <- Msg("<here we should send n>", mailbox); counterActor(n, mailbox) // TODO: Need a way to send the message.
            case Msg("INC", _)  =>
                if (n < 2147483647)
                    counterActor(n + 1, mailbox)
                else
                    NonResumableCrash("Counter overflowed.")
            case Msg("DEC", _)  => counterActor(n - 1, mailbox)
            case Msg(m, _)      => ResumableCrash(counterActor(n), "Unexpected message: " + m)
        }

    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Mailbox, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)           => c <- Ready(); workerActor(counterActor, mailbox)
            case Stop               => Done
            case Msg("wakeup", _)   => counterActor <- Msg("INC", mailbox); workerActor(counterActor, mailbox)
            case Msg(m, _)          => ResumableCrash(workerActor(counterActor), "Unexpected message: " + m)
        }

}
