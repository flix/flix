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
            actors = Map#{
                "helloWorld" -> helloWorld
            } | ActorSystem.default() }

    ///
    /// An actor that prints hello world and then terminates.
    ///
    pub def helloWorld(dir: ActorDirectory, m: Mailbox): ActorResult[Str] =
        match <- m {
            case Start(c)       =>
                c <- Ready;
                Console.printLine("Hello World!");
                Done
                // helloWorld(dir, m)
            case Stop           =>
                Console.printLine("Goodbye World!");
                Done
            case Msg(s, _)      => NonResumableCrash("Unexpected message: " + s)
        }

}
