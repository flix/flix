///
/// An actor system with an actor that crashes and is allowed to restart once.
///
namespace RestartOnce {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(100i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and multiple worker actors.
    ///
    /// The actor system uses default policies.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "unstable" -> unstableActor
        };
        let restartPolicy = #{
            // Compute the actors that have non-resumably crashed in the last second.
            Blocked(x) :-
                Log(x, Running, NonResumablyCrashed, timestamp),
                Clock(now),
                if timestamp `Instant.after` (Instant.minus(now, Duration.oneSecond())).

            Start(x) :- ActorState(x, NonResumablyCrashed), not Blocked(x).
        };
        { actors = actors, restartPolicy = restartPolicy | default }
    }

    ///
    /// An actor that explodes.
    ///
    pub def unstableActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("Ready...");
                c <- Ready;
                mailbox <- Msg("KABOOM", mailbox);
                unstableActor(dir, mailbox)
            case Stop => Done
            case Msg("KABOOM", _) =>
                Console.printLine("Kaboom!");
                NonResumableCrash("WHAM!")
            case Msg(msg, _)    => ResumableCrash(unstableActor, "Unexpected message: " + msg)
        }

}
