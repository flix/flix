namespace HelloWorld {

    ///
    /// The main entry point for the single actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with one hello world actor.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "helloWorld" -> helloWorld
        };
        { actors = actors | default }
    }

    ///
    /// An actor that prints hello world and then terminates.
    ///
    pub def helloWorld(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                Console.printLine("Hello World!");
                c <- Ready;
                helloWorld(dir, mailbox)
            case Stop =>
                Console.printLine("Goodbye World!");
                Done
            case Msg(msg, _) => ResumableCrash(helloWorld, "Unexpected message: " + msg)
        }

}
