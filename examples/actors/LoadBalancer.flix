
    ///
    /// An actor that occasionally requests the counter actor to increment its number.
    ///
//    pub def workerActor(dir: ActorDirectory, mailbox: Mailbox): ActorResult[Str] =
//        match <- mailbox {
//            case Start(c)           =>
//                let res = {
//                    let flatMap = Option.flatMap;
//                    let* m1 = sys.getActor("worker1", Running);
//                    let* m2 = sys.getActor("worker2", Running);
//                    let* m3 = sys.getActor("worker3", Running);
//                    let* m4 = sys.getActor("worker4", Running);
//                    m1 :: m2 :: m3 :: m4 :: Nil
//                };
//                match res {
//                    case None => NonResumableCrash("Where are my workers???")
//                    case Some(l) => c <- Ready; customLoadBalancer(l, sys, mailbox)
//                }
//            case Stop               => Done
//            case Msg(m, sender)          =>
//                match actors {
//                    case Nil => // cant happen // supposedly
//                    case x :: xs => x <- Msg(m, sender); customLoadBalancer(xs, List.rotateLeft(1, actors)), sys, mailbox)
//                }
//        }
