mod Test.Instance.Eq.Constraint.Super {

    trait Popable[t] {
        type Elm: Type
        type Aef: Eff
        pub def pop(b: t): Option[Popable.Elm[t]] \ Popable.Aef[t]
    }

    trait Replaceable[t] with Popable[t] {
        pub def replace(x: Popable.Elm[t], b: t): Option[Popable.Elm[t]] \ Popable.Aef[t]
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

    instance Replaceable[MutList[t, r]] with Iterable[t] where Iterable.Elm[t] ~ Int32 {
        pub def replace(x: t, b: MutList[t, r]): Option[t] \ r + Iterable.Aef[t] + IO =
            let popOpt = Popable.pop(b);
            if (Option.isEmpty(popOpt)) () else MutList.push(x, b);
            popOpt
    }

    @CompileTest
    pub def testPop(): Bool \ IO =
        region rc {
            let mList = MutList.empty(rc) !> MutList.push(Some(1));
            let popped = Popable.pop(mList);
            popped == Some(Some(1))
        }

    @CompileTest
    pub def testReplace(): Bool \ IO =
        region rc {
            let mList = MutList.empty(rc) !> MutList.push(Some(1));
            let old = Replaceable.replace(Some(2), mList);
            let noo = Popable.pop(mList);
            old == (Some(Some(1))) and noo == Some(Some(2))
        }

}
