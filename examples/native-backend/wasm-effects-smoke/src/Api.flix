mod Api {

    @Export
    pub def callCounter(seed: Int32): String \ Wit.HostDemoCounters =
        let c1 = Wit.counterNew(seed);
        let v1 = Wit.counterGet(c1);
        let v2 = Wit.counterAdd(c1, 5);
        let c2 = Wit.counterDuplicate(c1);
        let v3 = Wit.counterGet(c2);
        Wit.counterDrop(c1);
        Wit.counterDrop(c2);
        "${Int32.toString(v1)}:${Int32.toString(v2)}:${Int32.toString(v3)}"

}
