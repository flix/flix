
mod X {
    use A.Color
    use A.{Color => MyColor}

    pub def f(): Color = Color.Red
    pub def g(): Color = Color.Red
    pub def k(): MyColor = MyColor.Red
}

mod Y {
    use B.Kilogram
    use B.Acre

    pub def f(): Kilogram = Kilogram.Kilogram(123)
    pub def g(): Acre = 123
}

mod A {
    pub enum Color {
        case Red,
        case Blu
    }
}

mod B {
    pub enum Kilogram(Int32)
    pub type alias Acre = Int32
}
