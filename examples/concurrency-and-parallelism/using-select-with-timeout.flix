def sendMessage(s: Sender[String]): Unit \ {Chan, NonDet, IO} =
    Channel.recv(Channel.timeout(5, Time.TimeUnit.Milliseconds));
    Channel.send("Hello World", s)

def recvMessage(c: Receiver[String]): String \ {Chan, NonDet, IO} =
    let t = Channel.timeout(1, Time.TimeUnit.Milliseconds);
    select {
        case m <- recv(c) => m
        case _ <- recv(t) => "Timeout"
    }

def main(): Unit \ {Chan, NonDet, IO} =
    region rc {
        let (s, r) = Channel.buffered(1);
        spawn sendMessage(s) @ rc;
        println(recvMessage(r))
    }
