//def allTests(): Int = {
//    let x = 2;
//    x + 2;
//    x
//}
//
//def chanTest(): Channel[Float64] = chan Float64 100
//
//
//def getchTest(): (Int, Int) =
//    let c1 = chan Channel[Int] 100;
//    let c2 = chan Int 100;
//    c1 <- (c2 <- 1) <- (c2 <- 2);
//    let x = <- <- c1;
//    let y = <- <- c1;
//    (x, y)
//
//
//def putchTest(): Channel[Bool] = let c = chan Bool 100; c <- true
//
//def closechTest(): Channel[Int] = let c = chan Int 100; let c1 = closech c; c <- 1
//
//
//def collatz(n: Int): Int =
//    let c = chan Int 100;
//    c <- n;
//
//
//def slowFunction(n: Int): Int = collatz(n)
//
//def f(c: Channel[Int]): List[Channel[Int]] = List.map(x -> c <- x, List.range(1, 5))
//
//@test
//def spawnTest(): Int =
//    let c = chan Int 100;
//    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
//    spawn f(c); spawn f(c); spawn f(c); spawn f(c); spawn f(c);
//    <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c; <- c
//
//def spawnTest(): Str = let c = chan Str 100; spawn c <- "2"; <- c
//
//
//def selectTest(): Float64 = {
//    let c1 = chan Float64 100;
//    let asd = c1 <- 1.0;
//    let c2 = chan Float64 100;
//    let fgh = c2 <- 2.0;
//    select {
//        case x <- c2 => x + 1.0
//        case y <- c1 => y + 1.0
//    }
//}

//def statementTest(): Int = 1 ; 2
//
//def f(): Int = Console.printLine("te"); f()

//def test1(): Int = spawn f(); 2


//def f(c: Channel[Int]): Unit =
//  c <- 2;
//  ()

//def test2(): Int =
//  let c = chan Int 100;
//  spawn c <- 2;
//  <- c

//def sleepTest(): Bool = {
//  Console.printLine("1");
//  let c = chan Int 100;
//
//  spawn {
//    Console.printLine("2");
//    sleep(2000i64);
//    Console.printLine("3");
//    c <- 1;
//
//    Console.printLine("4")
////  ???
//  };
//
//  Console.printLine("5");
//
//  select {
//    case x <- c => Console.printLine("6")
//  };
//
//  Console.printLine("7");
//
//  let c1 = Timer.seconds(0);
//
//  sleep(5000i64);
//
//  select {
//    case x <- c => false
//    case y <- c1 => true
//  }
//}
//
//def sleepTest(): Unit = {
//  Console.printLine("1");
//  sleep(5i64 * Duration.oneSecond());
//  Console.printLine("2")
//}

//def defaultTest(): Int = {
//  let c = chan Int 100;
//  c <- 1;
//  select {
//    case x <- c => x
//    case _ => 3
//  }
//}

//def sizeTest(): Unit = {
//  let c = chan Int 1;
//  c <- 1;
//  Console.printLine("c <- 1");
//  spawn {
//    sleep(2i64 * Duration.oneSecond());
//    <- c;
//    Console.printLine("<- c")
//  };
//  c <- 2;
//  Console.printLine("c <- 2");
//  ()
//}

//def sizeTest(): Unit = {
//  let c = chan Int 1;
//  spawn {
//      select {
//              case x <- c => Console.printLine("<- c")
//              case _ => Console.printLine("default")
//            };
//      sleep(2i64 * Duration.oneSecond());
//      select {
//        case x <- c => Console.printLine("<- c")
//        case _ => Console.printLine("default")
//      }
//    };
//  sleep(1i64 * Duration.oneSecond());
//  c <- 1;
//  Console.printLine("c <- 1");
//  c <- 2;
//  Console.printLine("c <- 2");
//  ()
//}

//def tiktok(ticker: Ticker.Ticker, c: Channel[Int]): Unit = {
//  select {
//    case result <- c =>
//      Console.printLine("Result found");
//      ()
//    case tick <- Ticker.tick(ticker) => {
//      if(tick < 5) {
//        Console.printLine("tiktok");
//        tiktok(ticker, c)
//      }
//      else {
//        Console.printLine("Stop");
//        Ticker.stop(ticker)
//      }
//    }
//  }
//}
//
//def slowFunction(c: Channel[Int]): Unit = {
//  sleep(3i64 * Duration.oneSecond());
//   c <- 42 waits forever, and causes select tiktok() to halt too.
//  c <- 42;
//  ()
//}
//
//def tickerTest(): Unit = {
//  let ticker = Ticker.seconds(1i64);
//  let c = chan Int 1;
//  spawn slowFunction(c);
//  tiktok(ticker, c)
//}
//
// Should equal tickerTest, but Ticker is replaced with a regular channel and sleeping
//def selectTest(): Unit = {
//  let c1 = chan Int 5;
//  let c2 = chan Int 5;
//  let c3 = chan Int 5;
//  c2 <- 1 <- 2 <- 3 <- 4 <- 5;
//  spawn fillTick(c2);
//  spawn fill(c1);
//  sleep(5i64 * Duration.oneSecond());
//  sel(c1, c2, 0);
//  spawn {sleep(1i64 * Duration.oneSecond()); c2 <- 1; Console.printLine("c2 <- 1")};
//  spawn {select {
//    case x <- c1 => Console.printLine("c1")
//    case y <- c2 => Console.printLine("c2")
//  }; sleep(3i64 * Duration.oneHour())};
//  sleep(3i64 * Duration.oneSecond());
//  c2 <- 1;
//  sleep(3i64 * Duration.oneSecond());
//  spawn {sleep(1i64 * Duration.oneSecond()); c1 <- 1; Console.printLine("c1 <- 1")};
//  spawn {c1 <- 1; c3 <- 5};
//  sleep(3i64 * Duration.oneSecond());
//  c3 <- 6;
//  ()
//}
//
//def fill(c: Channel[Int]): Unit = {
//  sleep(3i64 * Duration.oneSecond());
//  c <- 1;
//  ()
//}
//
//def fillTick(c: Channel[Int]): Unit = {
//  sleep(800i64 * Duration.oneMillisecond());
//  c <- 1;
//  sleep(800i64 * Duration.oneMillisecond());
//  c <- 2;
//  sleep(800i64 * Duration.oneMillisecond());
//  c <- 3;
//  sleep(800i64 * Duration.oneMillisecond());
//  c <- 4;
//  sleep(800i64 * Duration.oneMillisecond());
//  c <- 5;
//  ()
//}
//
//def sel(c1: Channel[Int], c2: Channel[Int], i: Int): Unit = {
//  if (i < 1) {
//    select {
//      case x <- c1 => sel(c1, c2, i + 1)
//      case y <- c2 => Console.printLine("c2"); sel(c1, c2, i + 1)
//    }
//  } else {
//    Console.printLine("done")
//  }
//}
//
//
def blah(): Int = {
  let ch1 = chan Int 100;
  ch1 <- 1;
  let y = select {
    case x <- ch1 => x
  };
  y
}