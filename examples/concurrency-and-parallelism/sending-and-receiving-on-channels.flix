/// A function that sends every element of a list
def sendAll(l: List[Int32], s: Sender[Int32]): Unit \ Global =
    match l {
        case Nil     => ()
        case x :: xs => Channel.send(x, s); sendAll(xs, s)
    }

/// A function that receives n elements
/// and collects them into a list.
def recvN(n: Int32, r: Receiver[Int32]): List[Int32] \ Global + NonDet =
    match n {
        case 0 => Nil
        case _ => Channel.recv(r) :: recvN(n - 1, r)
    }

/// Spawn a process for send and wait, and print the result.
def main(): Unit \ Global + NonDet = {
    let l = 1 :: 2 :: 3 :: Nil;
    let (tx, rx) = Channel.buffered(100);
    spawn sendAll(l, tx);
    spawn recvN(List.length(l), rx)
}
