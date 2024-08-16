/// A struct datatype for a mutable stack
struct Stack[a, r] {
    r: Region[r],
    size: Int32,
    arr: Array[a, r],
    capacity: Int32
}

def newStack(rc: Region[r]): Stack[a, r] \ r =
    new Stack { r = rc, size = 0, arr = Array.empty(rc, 10), capacity = 10 } @ rc

def push(v: a, s: Stack[a, r]): Unit \ r =
    Array.put(v, s€size, s€arr);
    s€size = s€size + 1;
    if (s€size == s€capacity) {
        let newArr = Array.empty(s€r, s€capacity * 2);
        Array.forEachWithIndex(idx -> elem -> Array.put(elem, idx, newArr), s€arr);
        s€arr = newArr
    } else {
        ()
    }

def peek(s: Stack[a, r]): a \ r =
    Array.get(s€size - 1, s€arr)

def pop(s: Stack[a, r]): Unit \ r =
    s€size = s€size - 1
