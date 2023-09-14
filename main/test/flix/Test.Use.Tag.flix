mod Test.Use.Tag {

    @Test
    def testUseEnum01(): Bool =
        use Test.Use.Tag.A.Color.Red;
        match Red {
            case Red => true
            case _   => false
        }

    @Test
    def testUseEnum02(): Bool =
        use Test.Use.Tag.A.Color.Blu;
        match Blu {
            case Blu => true
            case _   => false
        }

    @Test
    def testUseEnum03(): Bool =
        use Test.Use.Tag.A.Color.Red;
        use Test.Use.Tag.A.Color.Blu;
        match Blu {
            case Red => false
            case Blu => true
        }

    @Test
    def testUseEnum04(): Bool =
        use Test.Use.Tag.A.Color.{Red, Blu};
        match Blu {
            case Red => false
            case Blu => true
        }

    @Test
    def testUseEnum05(): Bool =
        use Test.Use.Tag.A.Color.{Red => R};
        match R {
            case R => true
            case _ => false
        }

    @Test
    def testUseEnum06(): Bool =
        use Test.Use.Tag.A.Color.{Blu => B};
        match B {
            case B => true
            case _ => false
        }

    @Test
    def testUseEnum07(): Bool =
        use Test.Use.Tag.A.Color.{Red => R};
        use Test.Use.Tag.A.Color.{Blu => B};
        match R {
            case B => false
            case R => true
        }

    @Test
    def testUseEnum08(): Bool =
        use Test.Use.Tag.A.Color.{Red => R, Blu => B};
        match R {
            case B => false
            case R => true
        }

    @Test
    def testUseEnum09(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Red {
            case Color.Red => true
            case _         => true
        }

    @Test
    def testUseEnum10(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Blu {
            case Color.Blu => true
            case _         => true
        }

    @Test
    def testUseEnum11(): Bool =
        use Test.Use.Tag.A.Color;
        match Color.Red {
            case Color.Blu => false
            case Color.Red => true
        }

    @Test
    def testUseEnum12(): Bool =
        use Test.Use.Tag.A.B.Currency.USD;
        match USD {
            case USD => true
            case _   => false
        }

    @Test
    def testUseEnum13(): Bool =
        use Test.Use.Tag.A.B.Currency.CAD;
        match CAD {
            case CAD => true
            case _   => false
        }

    @Test
    def testUseEnum14(): Bool =
        use Test.Use.Tag.A.B.Currency.USD;
        use Test.Use.Tag.A.B.Currency.CAD;
        match USD {
            case USD => true
            case CAD => false
        }

    @Test
    def testUseEnum15(): Bool =
        use Test.Use.Tag.A.B.Currency.{USD, CAD};
        match USD {
            case USD => true
            case CAD => false
        }

    @Test
    def testUseEnum16(): Bool =
        use Test.Use.Tag.A.B.Currency;
        match Currency.USD {
            case Currency.USD => true
            case _            => false
        }

    @Test
    def testUseEnum17(): Bool =
        use Test.Use.Tag.A.B.Currency;
        match Currency.CAD {
            case Currency.CAD => true
            case _            => false
        }

    @Test
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

        @Test
        def testUseEnum19(): Bool =
            match Currency.USD {
                case Currency.CAD => false
                case Currency.USD => true
            }

    }
}
