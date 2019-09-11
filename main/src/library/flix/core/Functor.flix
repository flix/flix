//class Functor[m[a]] {
//    def map[b](f: a -> b, m: m[a]): m[b]
//}
//
//
//impl Functor[Option[a]] {
//    def map[b](f: a -> b, m: Option[a]): Option[b] = match m {
//        case None       => None
//        case Some(a)    => Some(f(a))
//    }
//}

//class Applicative[m[a]] <= Functor[m[a]] {
//
//}

//
// impl Applicative[Option[a]]
//

// TODO: Observation: The impl instantiates the type constructor m s.t. it becomes
// clear that Option[a] or Option[Int] must also be a Functor.

// can instance constraints be on type constructors?
// impl Eq[m[a]] <= Traverse[m[a]], Eq[a]
// looks ok?
