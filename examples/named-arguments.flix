// When functions have two arguments of the same type,
// it can be difficult to remember the right order.
// We can use named arguments to be sure we are applying the function correctly.
// The parameter type becomes a singleton record,
// and a syntactic sugar allows us to elide the braces when calling a function with a named argument.

///
/// Returns true if the `sublist` is contained within the list `l`.
///
pub def contains(sublist: {sublist = List[a]}, l: List[a]): Bool with Eq[a] = match (sublist.sublist, l) {
    case (Nil, _) => true
    case (_ :: _, Nil) => false
    case (h1 :: t1, h2 :: t2) if h1 == h2 => contains(sublist = t1, t2) or contains(sublist = sublist.sublist, t2)
    case (sub, _ :: t2) => contains(sublist = sub, t2)
}

// We often leave one of the parameters unnamed to allow for piping.

///
/// Returns true if the list contains my secret code.
///
pub def hasSecretCode(l: List[Int32]): Bool = {
    l |> contains(sublist = 1 :: 2 :: 3 :: 4 :: Nil)
}

// We can also use named arguments in enums, where labels are otherwise unnamed.

///
/// Represents a Flixball player.
///
pub enum Player({id = Int32}, {score = Int32})

///
/// Creates a new player from the given ID.
///
pub def initializePlayer(id: Int32): Player = Player.Player(id = id, score = 0)
