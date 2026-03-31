mod Test.Use.Tag {

    @CompileTest
    def testUseEnum01(): Bool =
        use Test.Use.Tag.A.Color.Red;
        match Red {
            case Red => true
            case _   => false
        }

    @CompileTest
    def testUseEnum02(): Bool =
        use Test.Use.Tag.A.Color.Blu;
        match Blu {
            case Blu => true
            case _   => false
        }

    @CompileTest
    def testUseEnum03(): Bool =
        use Test.Use.Tag.A.Color.Red;
        use Test.Use.Tag.A.Color.Blu;
        match Blu {
            case Red => false
            case Blu => true
        }

    @CompileTest
    def testUseEnum04(): Bool =
        use Test.Use.Tag.A.Color.{Red, Blu};
        match Blu {
            case Red => false
            case Blu => true
        }

    @CompileTest
    def testUseEnum05(): Bool =
        use Test.Use.Tag.A.Color.{Red => R};
        match R {
            case R => true
            case _ => false
        }

    @CompileTest
    def testUseEnum06(): Bool =
        use Test.Use.Tag.A.Color.{Blu => B};
        match B {
            case B => true
            case _ => false
        }

    @CompileTest
    def testUseEnum07(): Bool =
        use Test.Use.Tag.A.Color.{Red => R};
        use Test.Use.Tag.A.Color.{Blu => B};
        match R {
            case B => false
            case R => true
        }

    @CompileTest
    def testUseEnum08(): Bool =
        use Test.Use.Tag.A.Color.{Red => R, Blu => B};
        match R {
            case B => false
            case R => true
        }

    @CompileTest
    def testUseEnum09(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Red {
            case Color.Red => true
            case _         => true
        }

    @CompileTest
    def testUseEnum10(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Blu {
            case Color.Blu => true
            case _         => true
        }

    @CompileTest
    def testUseEnum11(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Red {
            case Color.Blu => false
            case Color.Red => true
        }

    @CompileTest
    def testUseEnum12(): Bool =
        use Test.Use.Tag.A.B.Currency.USD;
        match USD {
            case USD => true
            case _   => false
        }

    @CompileTest
    def testUseEnum13(): Bool =
        use Test.Use.Tag.A.B.Currency.CAD;
        match CAD {
            case CAD => true
            case _   => false
        }

    @CompileTest
    def testUseEnum14(): Bool =
        use Test.Use.Tag.A.B.Currency.USD;
        use Test.Use.Tag.A.B.Currency.CAD;
        match USD {
            case USD => true
            case CAD => false
        }

    @CompileTest
    def testUseEnum15(): Bool =
        use Test.Use.Tag.A.B.Currency.{USD, CAD};
        match USD {
            case USD => true
            case CAD => false
        }

    @CompileTest
    def testUseEnum16(): Bool =
        use Test.Use.Tag.A.B.Currency;
        match Currency.USD {
            case Currency.USD => true
            case _            => false
        }

    @CompileTest
    def testUseEnum17(): Bool =
        use Test.Use.Tag.A.B.Currency;
        match Currency.CAD {
            case Currency.CAD => true
            case _            => false
        }

    @CompileTest
    def testUseEnum18(): Bool =
        use Test.Use.Tag.A.B.Currency;
        match Currency.USD {
            case Currency.CAD => false
            case Currency.USD => true
        }

    mod A {
        pub enum Color {
            case Red,
            case Blu
        }
        mod B {
            pub enum Currency {
                case USD,
                case CAD
            }
        }
    }
    mod C {
        use Test.Use.Tag.A.B.Currency

        @CompileTest
        def testUseEnum19(): Bool =
            match Currency.USD {
                case Currency.CAD => false
                case Currency.USD => true
            }

    }
}
