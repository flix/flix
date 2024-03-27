// pub enum GGen[a: Type] {
//     case GGen(Rand -> a \ IO)
// }

// instance Functor[GGen] {
//     pub def map(f: a -> b \ ef, gen: GGen[a]): GGen[b] \ ef =
//         match gen {
//             case GGen.GGen(func) => checked_ecast(GGen.GGen(rand -> unchecked_cast(f(func(rand)) as _ \ IO)))
//         }
// }

// pub def main123(): Unit \ IO = 
    
//     println("Hello World!")

// pub def applyGGen(gen: GGen[a], rand: Rand): a = 
//     let GGen.GGen(func) = gen;
//     func(rand)



// instance Applicative[GGen] {

//     pub def pure(x: a): GGen[a] = GGen.GGen(rand -> x)

//     pub def ap(f: GGen[a -> b], x: GGen[a]): GGen[b] = 
//         ???

// }

// instance Monad[GGen] {

//     pub def flatMap(f: a -> GGen[b] \ ef, x: GGen[a]): GGen[b] \ ef = 
//         ???
// }