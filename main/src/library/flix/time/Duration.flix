namespace Duration {

    ///
    /// Returns the number of nanoseconds in one 24 hour day.
    ///
    pub def oneDay(): Int = 24 * oneHour()

    ///
    /// Returns the number of nanoseconds in one hour.
    ///
    pub def oneHour(): Int = 60 * oneMinute()

    ///
    /// Returns the number of nanoseconds in one minute.
    ///
    pub def oneMinute(): Int = 60 * oneSecond()

    ///
    /// Returns the number of nanoseconds in one second.
    ///
    pub def oneSecond(): Int = 1000 * oneMillisecond()

    ///
    /// Returns the number of nanoseconds in one millisecond.
    ///
    pub def oneMillisecond(): Int = 1000 * oneMicrosecond()

    ///
    /// Returns the number of nanoseconds in one microsecond.
    ///
    pub def oneMicrosecond(): Int = 1000 * oneNanosecond()

    ///
    /// Returns the number of nanoseconds in one nanosecond, i.e. one.
    ///
    pub def oneNanosecond(): Int = 1

}
