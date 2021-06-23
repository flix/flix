type alias IsFinished = Unit ~> Bool

type alias Peek[a] = Unit ~> a
    // TODO: Peek makes map incorrect or difficult?


type alias Pull[a] = Unit ~> a

pub enum ListIterator[a] {
    case ListIterator(IsFinished, Peek[a], Pull[a])
}

namespace ListIterator {

    pub def isEmpty(iter: ListIterator[a]): Bool & Impure =
        let ListIterator(isFinished, _peek, _pull) = iter;
        isFinished()

    pub def pull(iter: ListIterator[a]): a & Impure =
        let ListIterator(_isFinished, _peek, pull1) = iter;
        pull1()

    pub def count(f: a -> Bool, iter: ListIterator[a]): Int32 & Impure =
        if (isEmpty(iter))
            0
        else {
            if (f(pull(iter)))
                1 + count(f, iter) // TODO: Taill call
            else
                count(f, iter)
        }



    // TODO: mapE

    // NB: Pure (!!!)
    pub def mapL(f: a -> b, iter: ListIterator[a]): ListIterator[b] =
        let ListIterator(isFinished, peek, pull) = iter;
        let peek1 = () -> f(peek());
        let pull1 = () -> f(pull());
        ListIterator(isFinished, peek1, pull1)

    pub def filterL(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] & Impure =
        let ListIterator(_isFinished, peek, pull) = iter;
        let cursor = ref None;
        let isFinished1 = () -> match skipToNext(f, iter) {
            case None => true
            case Some(x) =>
                // TODO: Stash x.
                cursor := Some(x);
                false
        };
        let peek1 = () -> ???;
        let pull1 = () -> match deref cursor {
            case None => pull()
            case Some(x) =>
                cursor := None;
                x
        };
        ListIterator(isFinished1, peek1, pull1)

    // TODO: Make private
    pub def skipToNext(f: a -> Bool, iter: ListIterator[a]): Option[a] & Impure =
        let ListIterator(isFinished, _peek, pull) = iter;
        if (isFinished())
            None
        else {
            let n = pull();
            if (f(n)) Some(n) else skipToNext(f, iter)
        }

    pub def toIter(l: List[a]): ListIterator[a] & Impure =
        let curr = ref l;
        let isFinished = () -> List.isEmpty(deref curr);
        let peek = () -> match deref curr {
            case Nil    => bug!("Empty iterator.")
            case x :: _ => x
        };
        let pull = () -> match deref curr {
            case Nil     => bug!("Empty iterator.")
            case x :: xs =>
                curr := xs;
                x
        };
        ListIterator(isFinished, peek, pull)

    pub def toList(iter: ListIterator[a]): List[a] & Impure =
        let ListIterator(isFinished, _peek, pull) = iter;
        if (isFinished())
            Nil
        else
            pull() :: toList(iter)

    // TODO: The constant unwrapping of ListIterator is probably expensive.
    // TODO: Need opaque types.
    // TODO: Can we then introduce hasNext as separate functions?
    // TODO: Consider renaming HasNext to IsFinished.

}


def main(_args: Array[String]) : Int32 & Impure =
    let l = List.range(1, 10);
    let iter = ListIterator.toIter(l)
        |> ListIterator.filterL(x -> x % 2 == 0)
        |> ListIterator.mapL(x -> x * x);
    ListIterator.toList(iter) |> println;
    0
