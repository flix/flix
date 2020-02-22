@test
def testDaysTimer01(): Bool & Impure = select {
    case _ <- Timer.days(0i64) => true
}

@test
def testHoursTimer01(): Bool & Impure = select {
    case _ <- Timer.hours(0i64) => true
}

@test
def testMinutesTimer01(): Bool & Impure = select {
    case _ <- Timer.minutes(0i64) => true
}

@test
def testSecondsTimer01(): Bool & Impure = select {
    case _ <- Timer.seconds(0i64) => true
}

@test
def testMillisecondsTimer01(): Bool & Impure = select {
    case _ <- Timer.milliseconds(0i64) => true
}

@test
def testMicrosecondsTimer01(): Bool & Impure = select {
    case _ <- Timer.microseconds(0i64) => true
}

@test
def testNanosecondsTimer01(): Bool & Impure = select {
    case _ <- Timer.nanoseconds(0i64) => true
}
