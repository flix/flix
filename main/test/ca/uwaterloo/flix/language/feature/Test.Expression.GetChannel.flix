def getChannel01(): Int =
  let ch = channel Int 1;
  let _ = ch <- 2;
  <- ch

@test
def testGetChannel01(): Bool = getChannel01() `assertEq!` 2
