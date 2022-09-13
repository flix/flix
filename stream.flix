namespace Experimental/Stream {

  pub enum Stream[a: Type, fin: Bool] {
    case SNil
    case SCons(a, Lazy[Stream[a, fin]])
  }

  type alias Finite = true

  type alias Infinite = false

  pub def head(s: Stream[a, Infinite]): a = match s {
    case SNil        => ??? // TODO: impossible-- type system sound rule it out.
    case SCons(x, _) => x
  }

  pub def last(s: Stream[a, Finite]): Option[a] = match s {
    case SNil => None
    case SCons(x, lazy_xs) =>
      let xs = force lazy_xs;
      match xs {
        case SNil => Some(x)
        case SCons(_,_) => last(xs)
      }
  }

  pub def append(s1: Stream[a, Finite], s2: Stream[a, fin]): Stream[a, fin] = match s1 {
    case SNil => s2
    case SCons(x, xs) =>
      SCons(x, lazy append(force xs, s2))
  }

  pub def reverse(s: Stream[a, Finite]): Stream[a, Finite] = match s {
    case SNil => SNil
    case SCons(x, xs) => append(reverse(force xs), SCons(x, lazy SNil))
  }

  pub def map(f: a -> b, s: Stream[a, fin]): Stream[b, fin] = match s {
    case SNil => SNil
    case SCons(x, xs) => SCons(f(x), lazy map(f, force xs))
  }

  // should f be a -> Stream[b, *fin1*]? Then append does not work...
  // could use if(fin) {...} else {f(x)} since f(x) won't terminate the rest does not matter?
  pub def flatMap(f: a -> Stream[b, Finite], s: Stream[a, fin]): Stream[b, fin] = match s {
    case SNil => SNil
    case SCons(x, xs) => append(f(x), flatMap(f, force xs))
  }

  //Another version of flatMap that allows f to return infinite streams, but does not compile
  pub def flatMap2(f: a -> Stream[b, fin1], s: Stream[a, fin2]): Stream[b, fin1 and fin2] = match s {
    case SNil => SNil
    case SCons(x, xs) =>
      if(reifyBool fin1 == ReifiedFalse) {
        // f gives infinite lists
        //f(x)
        ??? // <- we know that fin1 is false, but it does not compile
      } else {
        // f gives finite lists
        //append(f(x), flatMap(f, force xs))
        ??? // <- we know that fin1 is true, but it does not compile...
      }
  }

  pub def zip(s1: Stream[a, fin1], s2: Stream[b, fin2]): Stream[(a, b), fin1 or fin2] = match (s1,s2) {
    case (SCons(x, xs), SCons(y, ys)) => SCons((x, y), lazy zip(force xs, force ys))
    case (_,_) => SNil // if one list ends, the zipping ends
  }

}