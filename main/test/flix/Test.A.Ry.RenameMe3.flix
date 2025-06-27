trait Popable[t] {
    type Elm
    type Aef: Eff
    pub def pop(b: t): Option[Popable.Elm[t]] \ Popable.Aef[t]
}

instance Popable[MutList[t, r]] with Iterable[t] where Iterable.Elm[t] ~ Int32 {
    type Elm = t
    type Aef = r + Iterable.Aef[t] + IO
    pub def pop(b: MutList[t, r]): Option[t] \ r + Iterable.Aef[t] + IO =
        let popped = MutList.pop(b);
        region rc {
            let sum = popped
                |> Option.map(Iterable.iterator(rc))
                |> Option.flatMap(Iterator.next)
                |> Option.sum; // Requires Option[Int32]
            println(sum);
            popped
        }
}

pub def demoPop(): Unit \ IO =
    region rc {
        let mList = MutList.empty(rc) !> MutList.push(Some(1));
        let popped = Popable.pop(mList);
        if (popped == Some(Some(1))) {
            println("Success!")
        } else {
            println("Fail")
        }
    }
