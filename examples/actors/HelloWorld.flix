namespace HelloWorld {

    ///
    /// The main entry point for the single actor system.
    ///
    @test
    pub def main(): Unit = ActorSystem.start(system()); ()

    ///
    /// Returns an actor system with one hello world actor.
    ///
    pub def system(): ActorSystem = {
            actors = #{
                ActorDef("helloWorld", helloWorld).
            } | ActorSystem.default() }

    ///
    /// An actor that prints hello world and then terminates.
    ///
    pub def helloWorld(mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       =>
                c <- Ready;
                Console.printLine("Hello World!");
                Done
            case Stop           => Done
            case Msg(m, _)      => FatalCrash("Unexpected message: " + m)
        }

}
