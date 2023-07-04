/// We can abuse Flix' type system to prove De Morgan's laws.

///
/// We define a new type indexed by a type-level Boolean:
///
enum B[_: Bool] with Eq {
    case B
}

///
/// We then define operations for negation, disjunction, and conjunction.
///
def mkNot(_: B[b]): B[not b] = B.B
def mkOr(_: B[a], _: B[b]): B[b or a] = B.B
def mkAnd(_: B[a], _: B[b]): B[b and a] = B.B

///
/// We encode De Morgan's laws as equations between terms.
/// But since == requires the LHS and RHS to have the same
/// types, this enforces that the formulas are the same.
///
/// Moreover, since the a's and b's are parameters they
/// must be kept symbolic (i.e. the type system will not
/// instantiate them to true or false).
///
def deMorgan01(x: B[a], y: B[b]): Bool =
    mkNot(mkAnd(x, y)) == mkOr(mkNot(x), mkNot(y))

def deMorgan02(x: B[a], y: B[b]): Bool =
    mkNot(mkOr(x, y)) == mkAnd(mkNot(x), mkNot(y))

/// Note that if we try to write a law that is invalid,
/// the type checker will fail:
/// def invalidLaw(x: B[a], y: B[b]): Bool =
///    x == mkNot(y)
