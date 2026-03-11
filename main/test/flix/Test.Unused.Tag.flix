mod Test.Unused.Tag {

    enum Color {
        case Red,
        case _Blu
    }

    enum Currency {
        case _USD,
        case CAD,
        case _DKK
    }

    pub def f(): Color = Color.Red

    pub def g(): Currency = Currency.CAD

}
