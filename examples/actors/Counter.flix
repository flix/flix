///
/// A simple actor system that consists of:
///
/// - 1x counterActor that maintains an internal counter.
/// - 2x workerActors that increment the counter.
///
namespace Counter {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one counter actor and multiple worker actors.
    ///
    /// The actor system uses default policies.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "counter" -> counterActor(0i32),
            "worker1" -> workerActor(None),
            "worker2" -> workerActor(None)
        };
        let dependencies = #{
            DependsOn("worker1", "counter").
            DependsOn("worker2", "counter").
        };
        { actors = actors, startPolicy = default.startPolicy <+> dependencies | default }
    }

    ///
    /// An actor that maintains an internal counter.
    ///
    /// The counter can be incremented, decrement, and its value retrieved.
    ///
    /// The actor non-resumably crashes if the counter overflows.
    /// The actor resumably crashes if the counter becomes negative.
    ///
    ///
    pub def counterActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => c <- Ready; counterActor(n, dir, mailbox)
            case Stop           => Done
            case Msg("GET", c)  =>
                c <- Msg("<here we should send n>", mailbox); // TODO: Here we should reply with the internal number.
                counterActor(n, dir, mailbox)
            case Msg("INC", _)  =>
                if (n < 2147483647)
                    counterActor(n + 1, dir, mailbox)
                else
                    NonResumableCrash("Counter overflowed.")
            case Msg("DEC", _)  =>
                if (n == 0)
                    ResumableCrash(counterActor(n), "Counter about to become negative.")
                else
                    counterActor(n - 1, dir, mailbox)
            case Msg(msg, _)      => ResumableCrash(counterActor(n), "Unexpected message: " + msg)
        }

    ///
    /// An actor that requests the counter actor to increment its number.
    ///
    pub def workerActor(counterActor: Option[Mailbox], dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
              let counter = ActorDirectory.getActor(dir, "counter", Running);
              // TODO: Quentin: If I uncomment these lines then the system gets stuck.
              //mailbox <- Msg("WAKEY-WAKEY", mailbox);
              //mailbox <- Msg("WAKEY-WAKEY", mailbox);
              //mailbox <- Msg("WAKEY-WAKEY", mailbox);
              c <- Ready();
              workerActor(counter, dir, mailbox)
            case Stop => Done
            case Msg("WAKEY-WAKEY", _) =>
                match counterActor {
                    case None    => NonResumableCrash("Where did the counterActor go?")
                    case Some(c) =>
                        c <- Msg("INC", mailbox);
                        workerActor(counterActor, dir, mailbox)
                }
            case Msg(msg, _)          => ResumableCrash(workerActor(counterActor), "Unexpected message: " + msg)
        }
}
