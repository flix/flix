///
/// An actor system with a single actor that performs a countdown and then explodes.
/// The actor system then restarts it for it to happen all over again.
///
namespace Countdown {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(50i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and multiple worker actors.
    ///
    /// The actor system uses default policies.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "countdown" -> countdownActor(11i32)
        };
        { actors = actors | default }
    }

    ///
    /// An actor that performs a countdown and then explodes.
    ///
    pub def countdownActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("Ready...");
                c <- Ready;
                mailbox <- Msg("TICK", mailbox);
                countdownActor(n, dir, mailbox)
            case Stop => Done
            case Msg("TICK", _) =>
                if (n == 0) {
                    Console.printLine("KABOOM!");
                    NonResumableCrash("KABOOM!")
                } else {
                    Console.printLine("Tick-Tock");
                    mailbox <- Msg("TICK", mailbox);
                    countdownActor(n - 1, dir, mailbox)
                }
            case Msg(msg, _) => ResumableCrash(countdownActor(n), "Unexpected message: " + msg)
        }

}
