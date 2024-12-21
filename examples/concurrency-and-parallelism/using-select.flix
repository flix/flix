def mooo(tx: Sender[String], n: Int32): Unit \ Chan =
    match n {
        case 0 => ()
        case x => Channel.send("Mooo!", tx); mooo(tx, x - 1)
    }

def meow(tx: Sender[String], n: Int32): Unit \ Chan =
    match n {
        case 0 => ()
        case x => Channel.send("Meow!", tx); meow(tx, x - 1)
    }

def main(): Unit \ {Chan, NonDet, IO} =
    region rc {
        let (s1, r1) = Channel.buffered(10);
        let (s2, r2) = Channel.buffered(10);
        spawn mooo(s1, 0) @ rc;
        spawn meow(s2, 3) @ rc;
        select {
            case m <- recv(r1) => println(m)
            case m <- recv(r2) => println(m)
        }
    }
