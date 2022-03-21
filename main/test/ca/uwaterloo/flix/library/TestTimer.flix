@test
def testDaysTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.days(0i64) => true
}

@test
def testHoursTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.hours(0i64) => true
}

@test
def testMinutesTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.minutes(0i64) => true
}

@test
def testSecondsTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.seconds(0i64) => true
}

@test
def testMillisecondsTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.milliseconds(0i64) => true
}

@test
def testMicrosecondsTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.microseconds(0i64) => true
}

@test
def testNanosecondsTimer01(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.nanoseconds(0i64) => true
}

@test
def testNanosecondsTimer02(): Bool & Impure = select {
    case _ <- Concurrent/Channel/Timer.nanoseconds(999999i64) => true
}
