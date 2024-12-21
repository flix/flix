def sendMessage(s: Sender[String]): Unit \ {Chan, IO} =
    use Time.Duration.fromMilliSeconds;
    Thread.sleep(fromMilliSeconds(5));
    Channel.send("Hello World", s)

def recvMessage(rx: Receiver[String]): Unit \ {Chan, NonDet, IO} =
    select {
        case m <- recv(rx) => println(m)
        case _             => println("No message was ready")
    }

def main(): Unit \ {Chan, NonDet, IO} =
    region rc {
        let (s, r) = Channel.buffered(1);
        spawn sendMessage(s) @ rc;
        spawn recvMessage(r) @ rc
    }
