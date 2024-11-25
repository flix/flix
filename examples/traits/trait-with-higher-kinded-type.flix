trait ForEach[t: Type -> Type] {
    pub def forEach(f: a -> Unit \ ef, x: t[a]): Unit \ ef
}

instance ForEach[Option] {
    pub def forEach(f: a -> Unit \ ef, o: Option[a]): Unit \ ef = match o {
        case None    => ()
        case Some(x) => f(x)
    }
}

instance ForEach[List] {
    pub def forEach(f: a -> Unit \ ef, l: List[a]): Unit \ ef = List.forEach(f, l)
}

def main(): Unit \ IO =
    let l = List.range(1, 100);
    ForEach.forEach(println, l)
