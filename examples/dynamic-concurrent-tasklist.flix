mod LoopingWorkQueue {
    use ChannelMsg.{More, Done}

    type alias WorkRes[t, w] = {res = Option[w], work = List[t]}

    type alias DualChannel[t: Type, r: Region] = (Sender[t, r], Receiver[t, r])

    enum ChannelMsg[t] with Sendable {
        case More(t)
        case Done
    }


    pub def main(workers: Int32, work: t -> WorkRes[t, res], x: t): List[res] \ IO with Sendable[t], Sendable[res] = region rc {
        let taskCountBuffer = 10_000;
        let tasksBuffer = 10_000;
        let resultsBuffer = 10_000;

        let (sendTaskCount, recvTaskCount) = Channel.buffered(rc, taskCountBuffer);
        let tasks = Channel.buffered(rc, tasksBuffer);
        let results = Channel.buffered(rc, resultsBuffer);
        let finalResultChannel = Channel.unbuffered(rc);

        spawn consumeResults(snd(results), Nil, fst(finalResultChannel)) @ rc;

        Channel.send(More(x), fst(tasks));
        Channel.send(1, sendTaskCount);

        List.range(0, workers) |> List.forEach( _ ->
            spawn startWorker(sendTaskCount, tasks, work, fst(results)) @ rc
        );

        def awaitTermination(count) = {
            let inc = Channel.recv(recvTaskCount);
            let newCount = count + inc;
            if (newCount != 0) awaitTermination(newCount)
            else ()
        };

        awaitTermination(0);

        Channel.send(Done, fst(results));

        let finalResult = Channel.recv(snd(finalResultChannel));

        List.range(0, workers) |> List.forEach( _ ->
            Channel.send(Done, fst(tasks))
        );

        finalResult
    }

    def startWorker(taskCount: Sender[Int32, r], tasks: DualChannel[ChannelMsg[t], r], work: t -> WorkRes[t, res], results: Sender[ChannelMsg[res], r]): Unit \ r with Sendable[res], Sendable[t] = {
        match Channel.recv(snd(tasks)) {
            case Done => () // terminate
            case More(x) =>
                let workRes = work(x);
                match (workRes.res) {
                    case None => ()
                    case Some(res) => Channel.send(More(res), results)
                };
                workRes.work |> List.forEach(w -> Channel.send(More(w), fst(tasks)));
                Channel.send(-1 + List.length(workRes.work), taskCount);
                startWorker(taskCount, tasks, work, results)
        }
    }

    def consumeResults(results: Receiver[ChannelMsg[t], r], acc: List[t], final: Sender[List[t], r]): Unit \ r with Sendable[t] = {
        match Channel.recv(results) {
            case Done => Channel.send(acc, final)
            case More(res) => consumeResults(results, res :: acc, final)
        }
    }

}

mod Example {
    pub def main(): Unit \ IO = {
        let init = "aheorw6b7c3awbo67c34ab68c4a68cawc34ay8lc34ay8c3ay8lcr3ay8lcr3awy8lcr3awy8lcr3awy8nlcr3awy8lcr3ay8nlcr3awy8lcr3ay8lcr3ay8nlcr3ay8lcr3ay8nlcr3ay8lcr3ay8nlcr3ay8nlcr3ay8nlcr3ay8nlcr3ay8lcr3ay8lcr3ay8lcr3ay8nlr3acr8ly8nry8cr3ay8";
        def work(s) = {
            let len = String.length(s);
            if (len <= 1) {res = Some(s), work = Nil}
            else {res = None, work = String.splitAt(len/2, s) |> (match (a, b) -> a :: b :: Nil)}
        };
        let chars = LoopingWorkQueue.main(42, work, init);
        println(chars)
    }
}
