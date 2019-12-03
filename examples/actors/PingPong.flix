namespace HelloWorld {

    ///
    /// The main entry point for the single actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with a ping actor and a pong actor.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "ping" -> pingActor(2),
            "pong" -> pongActor(2)
        };
        { actors = actors | default }
    }

    ///
    /// An actor that sends ping messages.
    ///
    pub def pingActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       =>
                c <- Ready;
                mailbox <- Msg("Pong", mailbox);
                pingActor(n, dir, mailbox)
            case Stop           => Done
            case Msg("Pong", _) if n == 0 =>
                Console.printLine("pingActor is done.");
                Done
            case Msg("Pong", _) =>
                Console.printLine("Received Pong!");
                match ActorDirectory.getActor(dir, "pong", Running) with {
                    case None => pingActor(n, dir, mailbox)
                    case Some(c) =>
                        c <- Msg("Ping", mailbox);
                        Console.printLine("Sent Ping!");
                        pingActor(n - 1, dir, mailbox)
                }
            case Msg(msg, _)      => ResumableCrash(pingActor(n), "Unexpected message: " + msg)
        }

    ///
    /// An actor that sends pong messages.
    ///
    pub def pongActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       =>
                c <- Ready;
                pongActor(n, dir, mailbox)
            case Stop           => Done
            case Msg("Ping", _) if n == 0 =>
                Console.printLine("pongActor is done.");
                Done
            case Msg("Ping", _) =>
                Console.printLine("Received Ping!");
                match ActorDirectory.getActor(dir, "ping", Running) with {
                    case None => pongActor(n, dir, mailbox)
                    case Some(c) =>
                        c <- Msg("Pong", mailbox);
                        Console.printLine("Sent Pong!");
                        pongActor(n - 1, dir, mailbox)
                }
            case Msg(msg, _)      => ResumableCrash(pingActor(n), "Unexpected message: " + msg)
        }

}
