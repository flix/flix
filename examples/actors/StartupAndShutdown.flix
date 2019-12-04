///
/// An example that demonstrates how actors can be started and stopped in an orderly fashion.
///
namespace StartupAndShutdown {

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
            "loadBalanceActor" -> loadBalanceActor,
            "webworker1" -> webWorkerActor,
            "webworker2" -> webWorkerActor,
            "webworker3" -> webWorkerActor,
            "loggingActor" -> loggingActor
        };
        // The actors are started and stopped according to their dependencies.
        let dependencies = #{
            DependsOn("databaseActor", "loggingActor").

            DependsOn("authenticationActor", "databaseActor").
            DependsOn("authenticationActor", "loggingActor").

            DependsOn("loadBalanceActor", "authenticationActor").
            DependsOn("loadBalanceActor", "databaseActor").
            DependsOn("loadBalanceActor", "loggingActor").

            DependsOn("webworker1", "loadBalanceActor").
            DependsOn("webworker2", "loadBalanceActor").
            DependsOn("webworker3", "loadBalanceActor").
        };
        { actors = actors,
          startPolicy = defaultStartPolicy() <+> dependencies,
          shutdownPolicy = defaultShutdownPolicy() <+> dependencies
          | default }
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
    /// A web worker actor.
    ///
    pub def webWorkerActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[WebWorker] Ready."); c <- Ready; webWorkerActor(dir, mailbox)
            case Stop           => Console.printLine("[WebWorker] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(webWorkerActor, "Unexpected message: " + msg)
        }

    ///
    /// A logging actor.
    ///
    pub def loggingActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       => Console.printLine("[Logging] Ready."); c <- Ready; loggingActor(dir, mailbox)
            case Stop           => Console.printLine("[Logging] Stopped."); Done
            case Msg(msg, _)    => ResumableCrash(loggingActor, "Unexpected message: " + msg)
        }

}
