@test
def testDaysTimer01(): Bool = select {
    case x <- Timer.days(0) => true
}

@test
def testHoursTimer01(): Bool = select {
    case x <- Timer.hours(0) => true
}

@test
def testMinutesTimer01(): Bool = select {
    case x <- Timer.minutes(0) => true
}

@test
def testSecondsTimer01(): Bool = select {
    case x <- Timer.seconds(0) => true
}

@test
def testMillisecondsTimer01(): Bool = select {
    case x <- Timer.milliseconds(0) => true
}

@test
def testMicrosecondsTimer01(): Bool = select {
    case x <- Timer.microseconds(0) => true
}

@test
def testNanosecondsTimer01(): Bool = select {
    case x <- Timer.nanoseconds(0) => true
}