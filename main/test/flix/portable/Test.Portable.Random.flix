mod Test.Portable.Random {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def seededSequence01(): Unit = {
        let (i32, i64, f32, f64, b) = Random.runWithSeed(123i64, () -> (
            Random.randomInt32(),
            Random.randomInt64(),
            Random.randomFloat32(),
            Random.randomFloat64(),
            Random.randomBool()
        ));

        let _ = assertTrue(i32 == -1188957731, "seededSequence01: randomInt32 mismatch.");
        let _ = assertTrue(i64 == 4_376_377_975_854_828_753i64, "seededSequence01: randomInt64 mismatch.");
        let _ = assertTrue(f32 == 0.30157375f32, "seededSequence01: randomFloat32 mismatch.");
        let _ = assertTrue(f64 == 0.25329310557439133f64, "seededSequence01: randomFloat64 mismatch.");
        assertTrue(b == true, "seededSequence01: randomBool mismatch.")
    }

    @Test
    def unseededRunWithIO01(): Unit \ IO = {
        let _ = Random.runWithIO(() -> Random.randomInt32());
        ()
    }

}
