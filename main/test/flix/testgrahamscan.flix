struct Stack[a, r] {
    mut rc: Region[r],
    mut size: Int32,
    mut arr: Array[a, r]
}


instance DotGet_hd[Array[a, r]] {
    type Aef = r
    type FieldType = a
    pub def get(arr: Array[a, r]): a \ r = Array.get(0, arr)
}

mod Stack {
    pub def newStack(rc: Region[r]): Stack[a, r] \ r =
        new Stack @ rc { rc = rc, size = 0, arr = Array.empty(rc, 10) }

    pub def pop(s: Stack[a, r]): Unit \ r =
        s.size = s.size - 1

    pub def peek(s: Stack[a, r]): a \ r =
        Array.get(s.size - 1, s.arr)

    pub def peek2(s: Stack[a, r]): a \ r =
        Array.get(s.size - 2, s.arr)

    pub def push(v: a, s: Stack[a, r]): Unit \ r =
        Array.put(v, s.size, s.arr);
        s.size = s.size + 1;
        if (s.size == Array.length(s.arr)) {
            let newArr = Array.empty(s.rc, Array.length(s.arr) * 2);
            Array.forEachWithIndex(idx -> elem -> Array.put(elem, idx, newArr), s.arr);
            s.arr = newArr
        } else {
            ()
        }
}

type alias Point = (Int32, Int32)

def crossProduct(p1: Point, p2: Point, p3: Point): Int32 =
    (p2.fst - p1.fst) * (p3.snd - p1.snd) -
      (p2.snd - p1.snd) * (p3.fst - p1.fst)

def sqDistance(p1: Point, p2: Point): Int32 =
    let dx = p1.fst - p2.fst;
    let dy = p1.snd - p2.snd;
    dx * dx + dy * dy

def compare_by_angle(pivot: Point, p1: Point, p2: Point): Comparison =
    let cross = crossProduct(pivot, p1, p2);
    if(cross == 0) {
        let dist1 = sqDistance(p1, pivot);
        let dist2 = sqDistance(p2, pivot);
        Order.compare(dist1, dist2)
    } else {
        Order.compare(0, cross)
    }

def findPivot(points: Array[Point, r]): Point \ r =
    let pivot = points.hd;
    match Array.minimumBy(compare_by_angle(pivot), points) {
        case Some(p) => p
        case None => pivot
    }


def initStack(points: Array[Point, r], rc: Region[r]): Stack[Point, r] \ r =
    let s = Stack.newStack(rc);
    Stack.push(points.hd, s);
    Stack.push(Array.get(1, points), s);
    s

def fixStack(stack: Stack[Point, r], p: Point): Unit \ r =
    if(stack.size > 1 and crossProduct(Stack.peek2(stack), Stack.peek(stack), p) < 0) {
        Stack.pop(stack);
        fixStack(stack, p)
    } else {
        ()
    }

def processPoints(rc: Region[r], stack: Stack[Point, r], points: Array[Point, r]): Unit \ r =
    if(not Array.isEmpty(points)) {
        let p = Array.get(0, points);
        fixStack(stack, p);
        Stack.push(p, stack);
        processPoints(rc, stack, Array.dropLeft(rc, 1, points))
    } else {()}

def grahamScan(rc: Region[r], points: Array[Point, r]): Array[Point, r] \ r =
    let pivot = findPivot(points);
    let sorted = Array.sortWith(rc, compare_by_angle(pivot), points);
    let stack = initStack(sorted, rc);
    processPoints(rc, stack, Array.dropLeft(rc, 2, sorted));
    stack.arr
