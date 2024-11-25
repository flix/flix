trait Equatable[t] {
    pub def equals(x: t, y: t): Bool
}

instance Equatable[Int32] {
    pub def equals(x: Int32, y: Int32): Bool = x == y
}

instance Equatable[String] {
    pub def equals(x: String, y: String): Bool = x == y
}

instance Equatable[Option[t]] with Equatable[t] {
    pub def equals(x: Option[t], y: Option[t]): Bool =
        match (x, y) {
            case (None, None)         => true
            case (Some(v1), Some(v2)) => Equatable.equals(v1, v2)
            case _                    => false
        }
}
