def collect(channel: Channel[Int], list: List[Int]): List[Int] = {
  select {
    case element <- channel => collect(channel, element :: list)
    case _ => list
  }
}

def main(): List[Int] = {
  let c = chan Int 4;
  c <- 1 <- 2 <- 3;
  collect(c, Nil)
}