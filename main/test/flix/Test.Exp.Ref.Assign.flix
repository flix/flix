namespace Test/Exp/Ref/Assign {

    @test
    def testAssignUnit01(): Bool & Impure =
        let l = ref ();
        l := ();
        deref l == ()

    @test
    def testAssignBool01(): Bool & Impure =
        let l = ref true;
        l := false;
        deref l == false

    @test
    def testAssignBool02(): Bool & Impure =
        let l = ref false;
        l := true;
        deref l == true

    @test
    def testAssignChar01(): Bool & Impure =
        let l = ref 'a';
        l := 'b';
        deref l == 'b'

    @test
    def testAssignFloat3201(): Bool & Impure =
        let l = ref 123.456f32;
        l := 456.123f32;
        deref l == 456.123f32

    @test
    def testAssignFloat6401(): Bool & Impure =
        let l = ref 123.456f64;
        l := 456.123f64;
        deref l == 456.123f64

    @test
    def testAssignInt801(): Bool & Impure =
        let l = ref 123i8;
        l := 111i8;
        deref l == 111i8

    @test
    def testAssignInt1601(): Bool & Impure =
        let l = ref 123i16;
        l := 456i16;
        deref l == 456i16

    @test
    def testAssignInt3201(): Bool & Impure =
        let l = ref 123i32;
        l := 456i32;
        deref l == 456i32

    @test
    def testAssignInt6401(): Bool & Impure =
        let l = ref 123i64;
        l := 456i64;
        deref l == 456i64

    @test
    def testAssignBigInt01(): Bool & Impure =
        let l = ref 111111111ii;
        l := 999999999ii;
        deref l == 999999999ii

    @test
    def testAssignStr01(): Bool & Impure =
        let l = ref "Hello World!";
        l := "Goodbye World!";
        deref l == "Goodbye World!"

    @test
    def testAssignOption01(): Bool & Impure =
        let l = ref None : Option[Int32];
        l := Some(123);
        deref l == Some(123)

    @test
    def testAssignOption02(): Bool & Impure =
        let l = ref Some(123);
        l := None;
        deref l == None

    @test
    def testAssignResult01(): Bool & Impure =
        let l = ref Ok(123);
        l := Err(456);
        deref l == Err(456)

    @test
    def testAssignResult02(): Bool & Impure =
        let l = ref Err(123);
        l := Ok(456);
        deref l == Ok(456)

    @test
    def testAssignAssign01(): Bool & Impure =
        let l = ref 1;
        l := 2;
        l := 3;
        deref l == 3

    @test
    def testAssignAssign02(): Bool & Impure =
        let l = ref 1;
        l := 2;
        l := 3;
        l := 4;
        deref l == 4

    @test
    def testAssignAssign03(): Bool & Impure =
        let l = ref 1;
        l := 2;
        l := 3;
        l := 4;
        l := 5;
        deref l == 5

    @test
    def testAssignAssign04(): Bool & Impure =
        let l = ref 1;
        l := deref l + 1;
        deref l == 2

    @test
    def testAssignAssign05(): Bool & Impure =
        let l = ref 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 3

    @test
    def testAssignAssign06(): Bool & Impure =
        let l = ref 1;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 4

    @test
    def testAssignAssign07(): Bool & Impure =
        let l = ref 1;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        l := deref l + 1;
        deref l == 5

    @test
    def testRefRef01(): Bool & Impure =
        let l = ref (ref 123);
        (deref l) := 456;
        deref (deref l) == 456

    @test
    def testRefRef02(): Bool & Impure =
        let l = ref (ref (ref 123));
        (deref (deref l)) := 456;
        deref (deref (deref l)) == 456

    @test
    def testAlias01(): Bool & Impure =
        let l1 = ref (ref 123);
        let l2 = ref 456;
        l1 := l2;
        deref (deref l1) == 456

    @test
    def testAlias02(): Bool & Impure =
        let l1 = ref (ref 123);
        let l2 = ref 456;
        l1 := l2;
        l2 := 789;
        deref (deref l1) == 789

}
