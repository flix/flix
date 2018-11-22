namespace Ticker {

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` days.
    ///
    pub def days(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneDay(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` hours.
    ///
    pub def hours(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneHour(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` minutes.
    ///
    pub def minutes(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneMinute(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` seconds.
    ///
    pub def seconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneSecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` milliseconds.
    ///
    pub def milliseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneMillisecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` microseconds.
    ///
    pub def microseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneMicrosecond(), c);
        c

    ///
    /// Returns a channel that receives the Unit value repeatedly every `n` nanoseconds.
    ///
    pub def nanoseconds(n: Int): Channel[Unit] =
        let c = chan Unit;
        spawn sleepSendAndRepeat(n * Duration.oneNanosecond(), c);
        c

    ///
    /// Sleeps for `s` nanoseconds, sends the Unit value to the given channel `c`, and then repeats.
    ///
    def sleepSendAndRepeat(s: Int, c: Channel[Unit]): Unit =
        sleep(s); c <- (); sleepSendAndRepeat(s, c)

    def sleep(l: Int): Unit = () // TODO: Replace by build in command.

}
