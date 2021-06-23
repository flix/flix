type alias HasNext = Unit ~> Bool

type alias Peek[a] = Unit ~> a

type alias Pull[a] = Unit ~> a

pub enum ListIterator[a] {
    case ListIterator(HasNext, Peek[a], Pull[a])
}

namespace ListIterator {

    pub def isEmpty(iter: ListIterator[a]): Bool & Impure =
        let ListIterator(hasNext, _peek, _pull) = iter;
        not hasNext()

    pub def count(f: a -> Bool, iter: ListIterator[a]): Int32 & Impure =
        let ListIterator(hasNext, _peek, pull) = iter;
        if (not hasNext())
            0
        else {
            if (f(pull()))
                1 + count(f, iter)
            else
                count(f, iter)
        }

    // TODO: Peek makes map incorrect or difficult?
    // NB: Pure (!!!)

    // TODO: mapE

    pub def mapL(f: a -> b, iter: ListIterator[a]): ListIterator[b] =
        let ListIterator(hasNext, peek, pull) = iter;
        let peek1 = () -> f(peek());
        let pull1 = () -> f(pull());
        ListIterator(hasNext, peek1, pull1)

    pub def filterL(f: a -> Bool, iter: ListIterator[a]): ListIterator[a] & Impure =
        let ListIterator(hasNext, peek, pull) = iter;
        let cursor = ref None;
        let hasNext1 = () -> match skipToNext(f, iter) {
            case None => false
            case Some(x) =>
                // TODO: Stash x.
                cursor := Some(x);
                true
        };
        let peek1 = () -> ???;
        let pull1 = () -> match deref cursor {
            case None => pull()
            case Some(x) =>
                cursor := None;
                x
        };
        ListIterator(hasNext1, peek1, pull1)

    // TODO: Make private
    pub def skipToNext(f: a -> Bool, iter: ListIterator[a]): Option[a] & Impure =
        let ListIterator(hasNext, _peek, pull) = iter;
        if (not hasNext())
            None
        else {
            let n = pull();
            if (f(n)) Some(n) else skipToNext(f, iter)
        }

    pub def toIter(l: List[a]): ListIterator[a] & Impure =
        let curr = ref l;
        let hasNext = () -> not List.isEmpty(deref curr);
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
        ListIterator(hasNext, peek, pull)

    pub def toList(iter: ListIterator[a]): List[a] & Impure =
        let ListIterator(hasNext, _peek, pull) = iter;
        if (not hasNext())
            Nil
        else
            pull() :: toList(iter)

    // TODO: The constant unwrapping of ListIterator is probably expensive.
    // TODO: Need opaque types.
    // TODO: Can we then introduce hasNext as separate functions?
    // TODO: Consider renaming HasNext to isEmpty.

}
