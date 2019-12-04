///
/// An example that demonstrates how actors can be started in an orderly fashion.
///
namespace OrderlyStartup {

    ///
    /// The main entry point of the actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        <- Timer.milliseconds(100i64);
        ActorSystem.shutdown(sys)

    ///
    /// Returns the actor system.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "authenticationActor" -> authenticationActor,
            "databaseActor" -> databaseActor,
            "loadBalanceActor" -> loadBalanceActor
        };
        { actors = actors | default }
    }

    ///
    /// An authentication actor.
    ///
    pub def authenticationActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[Authentication] Ready."); c <- Ready; authenticationActor(dir, mailbox)
            case Stop           => Console.printLine("[Authentication] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(authenticationActor, "Unexpected message: " + msg)
        }

    ///
    /// A database actor.
    ///
    pub def databaseActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[Database] Ready."); c <- Ready; databaseActor(dir, mailbox)
            case Stop           => Console.printLine("[Database] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(databaseActor, "Unexpected message: " + msg)
        }

    ///
    /// A load balance actor.
    ///
    pub def loadBalanceActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[LoadBalance] Ready."); c <- Ready; loadBalanceActor(dir, mailbox)
            case Stop           => Console.printLine("[LoadBalance] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(loadBalanceActor, "Unexpected message: " + msg)
        }




///
/// An actor system with several actors:
///
/// - An authentication actor.
/// - A database actor.
/// - A load balancer
///
/// - A logging actor.
/// - A performance monitor.
///

}
