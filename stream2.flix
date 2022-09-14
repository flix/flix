pub enum Stream[a: Type, fin: Bool, eval: Bool] {
    case SNil
    case SCons(a, Lazy[Stream[a, fin, eval]])
}

// false = infinite, true = finite
// false = eager, true = lazy

pub def head(s: Stream[a, false, _]): a = match s {
  case SNil        => ??? // TODO: impossible-- type system sound rule it out.
  case SCons(x, _) => x
}

pub def last(s: Stream[a, true, _]): Option[a] = match s {
  case SNil => None
  case SCons(x, lazy_xs) =>
    let xs = force lazy_xs;
    match xs {
      case SNil => Some(x)
      case SCons(_,_) => last(xs)
    }
}

// Only eval for s2 seems to matter, so  output uses that
pub def append(s1: Stream[a, true, _], s2: Stream[a, fin, eval]): Stream[a, fin, eval] = match s1 {
  case SNil => s2
  case SCons(x, xs) =>
    if (reifyBool eval == ReifiedFalse) {
      // Eager case.
      let tl = append(force xs, s2);
      SCons(x, lazy tl)
    } else {
      // Lazy case.
      SCons(x, lazy append(force xs, s2))
    }
}

// Nothing new since reverse depends entirely on append
pub def reverse(s: Stream[a, true, eval]): Stream[a, true, eval] = match s {
  case SNil => SNil
  case SCons(x, xs) =>
    append(reverse(force xs), SCons(x, lazy SNil))
}

pub def map(f: a -> b, s: Stream[a, fin, eval]): Stream[b, fin, eval] = match s {
  case SNil          => SNil
  case SCons(x, xs)  =>
    if (reifyBool eval == ReifiedFalse) {
      // Eager case
      if (reifyBool fin == ReifiedFalse) {
        // Infinite case
        ??? // A stream cannot be both eager and infinite
      } else {
        // Finite case
        let tl = map(f, force xs);
        SCons(f(x), lazy tl)
      }
    } else {
      // Lazy case (finite / infinite does not matter)
      SCons(f(x), lazy map(f, force xs))
    }
}

// should f be a -> Stream[b, *fin1*, eval]? Then append does not work...
// Only eval for f matters (because of append))
pub def flatMap(f: a -> Stream[b, true, eval], s: Stream[a, fin, _]): Stream[b, fin, eval] = match s {
  case SNil => SNil
  case SCons(x, xs) => append(f(x), flatMap(f, force xs))
}

//Output is lazy if either stream is lazy, and finite if either is finite
pub def zip(s1: Stream[a, fin1, eval1], s2: Stream[b, fin2, eval2]): Stream[(a, b), fin1 or fin2, eval1 or eval2] = match (s1,s2) {
  case (SCons(x, xs), SCons(y, ys)) =>
    if (reifyBool (eval1 or eval2) == ReifiedFalse) {
      // Both input streams are eager
      if (reifyBool (fin1 or fin2) == ReifiedFalse) {
        // Both input streams are infinite AND eager - this cannot happen
        ???
      } else {
        // At least one stream is finite
        let tl = zip(force xs, force ys);
        SCons((x,y), lazy tl)
      }
    } else {
      // At least one stream is lazy - finite / infinite does not matter
      SCons((x,y), lazy zip(force xs, force ys))
    }
  case (_,_) => SNil // If one list ends, zipping ends
}