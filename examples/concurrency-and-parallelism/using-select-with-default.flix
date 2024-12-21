def sendMessage(s: Sender[String]): Unit \ Chan =
    Channel.send("Hello World", s)

def recvMessage(rx: Receiver[String]): Unit \ {Chan, NonDet, IO} =
    select {
        case m <- recv(rx) => println(m)
        case _             => println("No message was ready")
    }

def main(): Unit \ {Chan, NonDet, IO} =
    region rc {
        let (s, r) = Channel.buffered(1);
        spawn recvMessage(r) @ rc;
        spawn sendMessage(s) @ rc
    }
