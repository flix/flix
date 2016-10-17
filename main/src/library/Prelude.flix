///
/// The Option type.
///
enum Option[t] {
    case None,
    case Some(t)
}

///
/// The Result type.
///
enum Result[t, e] {
    case Ok(t),
    case Err(e)
}

///
/// The List type.
///
enum List[t] {
    case Nil,
    case Cons(t, List[t])
}
