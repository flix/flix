/// A function that sends every element of a list
def send(s: Sender[Int32], l: List[Int32]): Unit \ IO =
    match l {
        case Nil     => ()
        case x :: xs => Channel.send(x, s); send(s, xs)
    }

/// A function that receives n elements
/// and collects them into a list.
def recv(r: Receiver[Int32], n: Int32): List[Int32] \ IO =
    match n {
        case 0 => Nil
        case _ => Channel.recv(r) :: recv(r, n - 1)
    }

/// Spawn a process for send and wait, and print the result.
def main(): Unit \ IO = {
    let l = 1 :: 2 :: 3 :: Nil;
    let (s, r) = Channel.buffered(100);
    spawn send(s, l);
    spawn recv(r, List.length(l))
}
