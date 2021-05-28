/// A function that sends every element of a list
def send(c: Channel[Int32], l: List[Int32]): Unit & Impure =
    match l {
        case Nil     => ()
        case x :: xs => c <- x; send(c, xs)
    }

/// A function that receives n elements
/// and collects them into a list.
def recv(c: Channel[Int32], n: Int32): List[Int32] & Impure =
    match n {
        case 0 => Nil
        case _ => (<- c) :: recv(c, n - 1)
    }

/// Spawn a process for send and wait, and print the result.
def main(_args: Array[String]): Int32 & Impure = {
    let l = 1 :: 2 :: 3 :: Nil;
    let c = chan Int 100;
    spawn send(c, l);
    spawn recv(c, List.length(l));
    0 // exit code
}
