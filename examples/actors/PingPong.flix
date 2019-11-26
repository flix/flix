namespace HelloWorld {

    ///
    /// The main entry point for the single actor system.
    ///
    // TODO: Enable @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one hello world actor.
    ///
    pub def system(): ActorSystem = {
            actors = #{
                Actor("pinger", pinger(p, 0)) :- MailboxOf("ponger", p).
                Actor("ponger", ponger(p, 0)) :- MailboxOf("pinger", p).
            } | ActorSystem.default() }

    ///
    /// An actor that sends ping messages.
    ///
    pub def pinger(other: Mailbox, n: Int, m: Mailbox): ActorResult[Str] =
        match <- m {
            case Start(c)       =>
                c <- Ready;
                pinger(other, n, m)
            case Stop           => Done
            case Msg("Pong", _) =>
                other <- Msg("Ping", m);
                pinger(other, n + 1, m)
            case Msg(s, _)      => NonResumableCrash("Unexpected message: " + s)
        }

    ///
    /// An actor that sends pong messages.
    ///
    pub def ponger(other: Mailbox, n: Int, m: Mailbox): ActorResult[Str] =
        match <- m {
            case Start(c)       =>
                c <- Ready;
                ponger(other, n, m)
            case Stop           => Done
            case Msg("Ping", _) =>
                other <- Msg("Pong", m);
                ponger(other, n + 1, m)
            case Msg(s, _)      => NonResumableCrash("Unexpected message: " + s)
        }

}
