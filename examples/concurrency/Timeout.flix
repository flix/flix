def main(): Option[Int] = {
  let c = chan Int 1;
  spawn slowFunction(43, c);
  let result = select {
    case result <- c => Some(result)
    case t <- Timer.seconds(1i64) => None
  };
  result
}

def slowFunction(input: Int, output: Channel[Int]): Unit = {
  let result = input;
  sleep(5000i64*Duration.oneMillisecond());
  output <- result;
  ()
}