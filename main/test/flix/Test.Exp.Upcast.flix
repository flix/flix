namespace Test/Exp/Upcast {

    /*

    @test
    def testUpcast01(): Unit & Impure =
        upcast ()
    @test
    def testUpcast02(): Unit = // Should trigger error
        upcast ()

    */

    @test
    def testUpcast03(): Unit =
      let f = x -> x + 1;
      let g = x -> { x + 1 as & Impure };
      let _ = if (true) upcast f else g;
      ()

}
