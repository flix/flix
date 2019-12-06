///
/// An actor system with an actor that sends pings to a load balancer.
///
namespace PingPong {

    ///
    /// The main entry point for the single actor system.
    ///
    @test
    pub def main(): Unit =
        let sys = ActorSystem.start(system());
        // TODO: this can take a fair amount of time to initialize, maybe we want ActorSystem.start to notify us when the system is fully started?
        <- Timer.milliseconds(1000i64); // wait for initialization
        let c = Channel.new();
        if (ActorSystem.send(sys, "ping", Msg("Start", c))) {
          <- c; // wait for finalization
          ()
        } else {
          ()
        };
        ActorSystem.shutdown(sys)

    ///
    /// Returns an actor system with a ping actor and a pong actor.
    ///
    pub def system(): ActorSystem = {
        let default = ActorSystem.default();
        let actors = Map#{
            "ping" -> pingActor(5),
            "pongMaster" -> loadBalancer("pong1" :: "pong2" :: "pong3" :: Nil),
            "pong1" -> pongActor,
            "pong2" -> pongActor,
            "pong3" -> pongActor
        };
        let dependencies = #{
            DependsOn("pongMaster", "ping").
            DependsOn("pongMaster", "pong1").
            DependsOn("pongMaster", "pong2").
            DependsOn("pongMaster", "pong3").
        };
        { actors = actors, startPolicy = default.startPolicy <+> dependencies | default }
    }

    ///
    /// An actor that sends ping messages.
    ///
    pub def pingActor(n: Int, dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       =>
                c <- Ready;
                pingActor(n, dir, mailbox)
            case Stop           => Done
            case Msg("Start", c) =>
                 mailbox <- Msg("Pong", c);
                 pingActor(n, dir, mailbox)
            case Msg("Pong", c) if n == 0 =>
                Console.printLine("pingActor is done.");
                c <- Msg("Done", c); // notifies that we're done
                Done
            case Msg("Pong", c) =>
                Console.printLine("Received Pong!");
                match ActorDirectory.getActor(dir, "pongMaster", Running) with {
                    case None => pingActor(n, dir, mailbox)
                    case Some(mb) =>
                        mb <- Msg("Ping", c);
                        Console.printLine("Sent Ping!");
                        pingActor(n - 1, dir, mailbox)
                }
            case Msg(msg, _)      => ResumableCrash(pingActor(n), "Unexpected message: " + msg)
        }

    ///
    /// An actor that sends pong messages when it receives ping messages, indefinitely.
    ///
    pub def pongActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c)       =>
                c <- Ready;
                pongActor(dir, mailbox)
            case Stop           => Done
            case Msg("Ping", c) =>
                Console.printLine("Received Ping!");
                match ActorDirectory.getActor(dir, "ping", Running) with {
                    case None => pongActor(dir, mailbox)
                    case Some(mb) =>
                        mb <- Msg("Pong", c);
                        Console.printLine("Sent Pong!");
                        pongActor(dir, mailbox)
                }
            case Msg(msg, _)      => ResumableCrash(pongActor, "Unexpected message: " + msg)
        }

    pub def loadBalancer(actorNames: List[Str], dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(c) =>
                let actors = List.foldLeft((acc, name) ->
                  match ActorDirectory.getActor(dir, name, Running) {
                      case Some(mb) => mb :: acc
                      case None => acc
                  }, Nil, actorNames);
                c <- Ready;
                if (List.isEmpty(actors)) {
                    NonResumableCrash("Where are my workers?")
                } else {
                    loadBalancerInitialized(actors, dir, mailbox)
                }
            case Stop => Done
            case Msg(_, _) => NonResumableCrash("I haven't been initialized properly!")
        }
    pub def loadBalancerInitialized(actors: List[Mailbox], dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
        match <- mailbox {
            case Start(_) => NonResumableCrash("Wrong initialization")
            case Stop => Done
            case Msg(m, c) =>
                match actors {
                    case Nil => NonResumableCrash("Where are my workers?")
                    case worker :: _ =>
                        worker <- Msg(m, c);
                        loadBalancerInitialized(List.rotateLeft(1, actors), dir, mailbox)
                }
        }
}
