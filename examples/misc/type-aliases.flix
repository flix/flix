/// A type alias introduces a short-hand for an existing type.

/// A type alias for the type Map[Int32, String].
type alias M = Map[Int32, String]

/// A function that returns a map of type M.
def f(): M = Map#{ 1 => "Hello", 2 => "World"}

// A polymorphic type alias for the type Map[a, Result[String, Int32]].
type alias N[a] = Map[a, Result[String, Int32]]

/// A function that returns a map of type N.
def g(): N[Int32] = Map#{ 1 => Ok(123), 2 => Err("Hello") }

/// Another function that returns a map of type N.
def h(): N[Bool] = Map#{ true => Ok(456) }
