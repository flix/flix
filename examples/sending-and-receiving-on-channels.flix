/// A function that sends every element of a list
def sendAll(l: List[Int32], s: Sender[Int32, r]): Unit \ r + IO =
    match l {
        case Nil     => ()
        case x :: xs => Channel.send(x, s); sendAll(xs, s)
    }

/// A function that receives n elements
/// and collects them into a list.
def recvN(n: Int32, r: Receiver[Int32, r]): List[Int32] \ r + IO =
    match n {
        case 0 => Nil
        case _ => Channel.recv(r) :: recvN(n - 1, r)
    }

/// Spawn a process for send and wait, and print the result.
def main(): Unit \ IO = region rc {
    let l = 1 :: 2 :: 3 :: Nil;
    let (tx, rx) = Channel.buffered(rc, 100);
    spawn sendAll(l, tx) @ rc;
    spawn recvN(List.length(l), rx) @ rc
}
