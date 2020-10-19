namespace Test/Exp/Fixpoint/Fold {

    /////////
    // Int //
    /////////
    def add(entry: (Int, Int), acc: Int): Int =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold01(): Int =
      let p = #{F(0, 1). F(1, 1).};
      fold F 0 add (solve p)

    @test
    def testFixpointFold02(): Int =
      let p = #{F(0, 1). F(1, 1).};
      fold F 0 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold03(): Bool =
      let p = #{F(0, 1). F(1, 1).};
      let res = fold F 0 add (solve p);
      res == 3

    @test
    def testFixpointFold04(): Bool =
      let p = #{F(0, 1). F(1, 1).};
      (fold F 0 add (solve p)) == 3

    //////////
    // Bool //
    //////////
    def fb(entry: (Bool, Bool), acc: Bool): Bool =
      let (x, y) = entry;
      acc || x || y

    @test
    def testFixpointFold05(): Bool =
      let p = #{Fb(false, true). Fb(true, true).};
      fold Fb false fb (solve p)

    @test
    def testFixpointFold06(): Bool =
      let p = #{Fb(false, false). Fb(true, false).};
      !(fold Fb false ((_entry, acc) -> acc) (solve p))

    //////////
    // Int8 //
    //////////
    def addi8(entry: (Int8, Int8), acc: Int8): Int8 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold07(): Int8 =
      let p = #{Fi8(0i8, 1i8). Fi8(1i8, 1i8).};
      fold Fi8 0i8 addi8 (solve p)

    @test
    def testFixpointFold08(): Int8 =
      let p = #{Fi8(0i8, 1i8). Fi8(1i8, 1i8).};
      fold Fi8 0i8 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold09(): Bool =
      let p = #{Fi8(0i8, 2i8). Fi8(1i8, 1i8). Fi8(1i8, 0i8).};
      let res = fold Fi8 0i8 addi8 (solve p);
      res == 5i8

    @test
    def testFixpointFold10(): Bool =
      let p = #{Fi8(0i8, 1i8). Fi8(1i8, 1i8).};
      (fold Fi8 0i8 addi8 (solve p)) == 3i8

    ///////////
    // Int16 //
    ///////////
    def addi16(entry: (Int16, Int16), acc: Int16): Int16 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold11(): Int16 =
      let p = #{Fi16(0i16, 1i16). Fi16(1i16, 1i16).};
      fold Fi16 0i16 addi16 (solve p)

    @test
    def testFixpointFold12(): Int16 =
      let p = #{Fi16(0i16, 1i16). Fi16(1i16, 1i16).};
      fold Fi16 0i16 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold13(): Bool =
      let p = #{Fi16(0i16, 2i16). Fi16(1i16, 1i16). Fi16(1i16, 0i16).};
      let res = fold Fi16 0i16 addi16 (solve p);
      res == 5i16

    @test
    def testFixpointFold14(): Bool =
      let p = #{Fi16(0i16, 1i16). Fi16(1i16, 1i16).};
      (fold Fi16 0i16 addi16 (solve p)) == 3i16

    ///////////
    // Int32 //
    ///////////
    def addi32(entry: (Int32, Int32), acc: Int32): Int32 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold15(): Int32 =
      let p = #{Fi32(0i32, 1i32). Fi32(1i32, 1i32).};
      fold Fi32 0i32 addi32 (solve p)

    @test
    def testFixpointFold16(): Int32 =
      let p = #{Fi32(0i32, 1i32). Fi32(1i32, 1i32).};
      fold Fi32 0i32 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold17(): Bool =
      let p = #{Fi32(0i32, 2i32). Fi32(1i32, 1i32). Fi32(1i32, 0i32).};
      let res = fold Fi32 0i32 addi32 (solve p);
      res == 5i32

    @test
    def testFixpointFold18(): Bool =
      let p = #{Fi32(0i32, 1i32). Fi32(1i32, 1i32).};
      (fold Fi32 0i32 addi32 (solve p)) == 3i32

    ///////////
    // Int64 //
    ///////////
    def addi64(entry: (Int64, Int64), acc: Int64): Int64 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold19(): Int64 =
      let p = #{Fi64(0i64, 1i64). Fi64(1i64, 1i64).};
      fold Fi64 0i64 addi64 (solve p)

    @test
    def testFixpointFold20(): Int64 =
      let p = #{Fi64(0i64, 1i64). Fi64(1i64, 1i64).};
      fold Fi64 0i64 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold21(): Bool =
      let p = #{Fi64(0i64, 2i64). Fi64(1i64, 1i64). Fi64(1i64, 0i64).};
      let res = fold Fi64 0i64 addi64 (solve p);
      res == 5i64

    @test
    def testFixpointFold22(): Bool =
      let p = #{Fi64(0i64, 1i64). Fi64(1i64, 1i64).};
      (fold Fi64 0i64 addi64 (solve p)) == 3i64

    ////////////
    // BigInt //
    ////////////
    def addbi(entry: (BigInt, BigInt), acc: BigInt): BigInt =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold23(): BigInt =
      let p = #{Fii(0ii, 1ii). Fii(1ii, 1ii).};
      fold Fii 0ii addbi (solve p)

    @test
    def testFixpointFold24(): BigInt =
      let p = #{Fii(0ii, 1ii). Fii(1ii, 1ii).};
      fold Fii 0ii ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold25(): Bool =
      let p = #{Fii(0ii, 2ii). Fii(1ii, 1ii). Fii(1ii, 0ii).};
      let res = fold Fii 0ii addbi (solve p);
      res == 5ii

    @test
    def testFixpointFold26(): Bool =
      let p = #{Fii(0ii, 1ii). Fii(1ii, 1ii).};
      (fold Fii 0ii addbi (solve p)) == 3ii


    /////////////
    // Float32 //
    /////////////
    def addf32(entry: (Float32, Float32), acc: Float32): Float32 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold27(): Float32 =
      let p = #{Ff32(0.0f32, 1.0f32). Ff32(1.0f32, 1.0f32).};
      fold Ff32 0.0f32 addf32 (solve p)

    @test
    def testFixpointFold28(): Float32 =
      let p = #{Ff32(0.0f32, 1.0f32). Ff32(1.0f32, 1.0f32).};
      fold Ff32 0.0f32 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold29(): Bool =
      let p = #{Ff32(0.0f32, 2.0f32). Ff32(1.0f32, 1.0f32). Ff32(1.0f32, 0.0f32).};
      let res = fold Ff32 0.0f32 addf32 (solve p);
      (res - 5.0f32) < 0.001f32

    @test
    def testFixpointFold30(): Bool =
      let p = #{Ff32(0.0f32, 1.0f32). Ff32(1.0f32, 1.0f32).};
      ((fold Ff32 0.0f32 addf32 (solve p)) - 3.0f32) < 0.001f32

    /////////////
    // Float64 //
    /////////////
    def addf64(entry: (Float64, Float64), acc: Float64): Float64 =
      let (x, y) = entry;
      acc + x + y

    @test
    def testFixpointFold31(): Float64 =
      let p = #{Ff64(0.0f64, 1.0f64). Ff64(1.0f64, 1.0f64).};
      fold Ff64 0.0f64 addf64 (solve p)

    @test
    def testFixpointFold32(): Float64 =
      let p = #{Ff64(0.0f64, 1.0f64). Ff64(1.0f64, 1.0f64).};
      fold Ff64 0.0f64 ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold33(): Bool =
      let p = #{Ff64(0.0f64, 2.0f64). Ff64(1.0f64, 1.0f64). Ff64(1.0f64, 0.0f64).};
      let res = fold Ff64 0.0f64 addf64 (solve p);
      (res - 5.0f64) < 0.001f64

    @test
    def testFixpointFold34(): Bool =
      let p = #{Ff64(0.0f64, 1.0f64). Ff64(1.0f64, 1.0f64).};
      ((fold Ff64 0.0f64 addf64 (solve p)) - 3.0f64) < 0.001f64

    ////////////
    // String //
    ////////////
    def adds(entry: (String, String), _acc: String): String =
      let (x, _) = entry;
      x

    @test
    def testFixpointFold35(): String =
      let p = #{Fs("foo", "bar"). Fs("baz", "qux").};
      fold Fs "" adds (solve p)

    @test
    def testFixpointFold36(): String =
      let p = #{Fs("foo", "bar"). Fs("baz", "qux").};
      fold Fs "" ((_entry, acc) -> acc) (solve p)

    @test
    def testFixpointFold37(): Bool =
     let p = #{Fs("foo", "bar").};
     let res = fold Fs "" adds (solve p);
     res == "foo"

    @test
    def testFixpointFold38(): Bool =
      let p = #{Fs("foo", "bar"). Fs("baz", "qux").};
      let res = fold Fs "" adds (solve p);
      !String.isEmpty(res)

    ////////////
    // Option //
    ////////////
    def addo(entry: (Option[Int], Option[Int]), acc: Option[Int]): Option[Int] =
      let (x, y) = entry;
      Option.withDefault(x, Option.withDefault(y, acc))

    @test
    def testFixpointFold39(): Bool =
      let p = #{Fo(None, None). Fo(None, Some(4)).};
      let res = fold Fo None addo (solve p);
      Option.getWithDefault(res, -1) == 4

    @test
    def testFixpointFold40(): Bool =
      let p = #{Fo(Some(1), Some(3)). Fo(Some(2), None).};
      let res = fold Fo None addo (solve p);
      !Option.isEmpty(res)

    ////////////
    // Result //
    ////////////
    def addr(entry: (Result[Int, String], Result[Int, String]), acc: Result[Int, String]): Result[Int, String] =
      let (x, y) = entry;
      Result.withDefault(x, Result.withDefault(y, acc))

    @test
    def testFixpointFold41(): Bool =
      let p = #{Fr(Err("foo"), Err("bar")). Fr(Err("baz"), Ok(4)).};
      let res = fold Fr Ok(0) addr (solve p);
      Result.getWithDefault(res, -1) == 4

    @test
    def testFixpointFold42(): Bool =
      let p = #{Fr(Ok(1), Ok(3)). Fr(Ok(2), Err("foo")).};
      let res = fold Fr Err("foo") addr (solve p);
      Result.isOk(res)

    @test
    def testFixpointFold43(): Int =
      let p = #{Fm("foo", 1). Fm("bar", 2).};
      fold Fm 0 ((entry, acc) ->
         let (_x, y) = entry;
         acc + y)
         (solve p)

    @test
    def testFixpointFold44(): Unit =
      fold U () ((x, _acc) -> x) (solve #{U().})

    @test
    def testFixpointFold45(): Bool =
      fold UnaryB false ((x, acc) -> x || acc) (solve #{UnaryB(true). UnaryB(false).})

    @test
    def testFixpointFold46(): Bool =
      fold UnaryS false ((x, acc) -> acc || x == "bar") (solve #{UnaryS("foo"). UnaryS("bar"). UnaryS("baz").})


    // Fold over multiple relations
    @test
    def testFixpointFold47(): Bool =
      let res = fold F 0 ((pair, acc) -> acc + fst(pair)) (solve #{F(1, 2). UnaryS("foo").});
      res == 1

}
