def sendMessage(s: Sender[String]): Unit \ Chan =
    Channel.send("Hello World!", s)

def recvMessage(r: Receiver[String]): Unit \ {Chan, NonDet, IO} =
    let msg = Channel.recv(r);
    println(msg)

def main(): Unit \ {Chan, NonDet, IO} = region rc {
    let (s, r) = Channel.unbuffered();
    spawn sendMessage(s) @ rc;
    spawn recvMessage(r) @ rc
}
