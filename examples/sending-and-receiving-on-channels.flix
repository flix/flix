/// A function that sends every element of a list
def send(o: Channel[Int], l: List[Int]): Unit & Impure =
    match l {
        case Nil     => ()
        case x :: xs => o <- x; send(o, xs)
    }

/// A function that receives n elements
/// and collects them into a list.
def recv(i: Channel[Int], n: Int): List[Int] & Impure =
    match n {
        case 0 => Nil
        case _ => (<- i) :: recv(i, n - 1)
    }

/// A function that calls receive and sends the result on d.
def wait(i: Channel[Int], n: Int, d: Channel[List[Int]]): Unit & Impure =
    d <- recv(i, n);
    ()

/// Spawn a process for send and wait, and print the result.
def main(_args: Array[String]): Int32 & Impure = {
    let l = 1 :: 2 :: 3 :: Nil;
    let c = chan Int 100;
    let d = chan List[Int] 100;
    spawn send(c, l);
    spawn wait(c, List.length(l), d);
    (<- d) |> println;
    0 // exit code
}
