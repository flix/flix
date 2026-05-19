def mismatchedClockAndRegion(s: MutSet[Int32, r]): Unit \ { Time.Clock } =
    let _ = MutSet.size(s);
    ()
