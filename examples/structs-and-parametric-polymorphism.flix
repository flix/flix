/// A struct datatype for a mutable stack
struct Stack[a, r] {
    rc: Region[r],
    mut size: Int32,
    mut arr: Array[a, r]
}

mod Stack {
    pub def newStack(rc: Region[r]): Stack[a, r] \ r =
        new Stack @ rc { rc = rc, size = 0, arr = Array.empty(rc, 10) }

    pub def pop(s: Stack[a, r]): Unit \ r =
        s->size = s->size - 1

    pub def peek(s: Stack[a, r]): Option[a] \ r =
        if(s->size > 0) {
            Some(Array.get(s->size - 1, s->arr))
        } else {
            None
        }

    pub def push(v: a, s: Stack[a, r]): Unit \ r =
        Array.put(v, s->size, s->arr);
        s->size = s->size + 1;
        if (s->size == Array.length(s->arr)) {
            let newArr = Array.empty(s->rc, Array.length(s->arr) * 2);
            Array.forEachWithIndex(idx -> elem -> Array.put(elem, idx, newArr), s->arr);
            s->arr = newArr
        } else {
            ()
        }
}
