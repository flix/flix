mod Test.Portable.MutCollections {

    def assertTrue(cond: Bool, msg: String): Unit =
        if (cond) () else bug!(msg)

    @Test
    def mutList01(): Unit = region rc {
        let l = MutList.empty(rc);
        MutList.push(10, l);
        MutList.push(20, l);
        let _ = assertTrue(MutList.length(l) == 2, "mutList01 failed: expected length == 2.");
        match MutList.pop(l) {
            case Some(v) => assertTrue(v == 20, "mutList01 failed: expected pop == Some(20).")
            case None => bug!("mutList01 failed: expected Some.")
        };
        match MutList.pop(l) {
            case Some(v) => assertTrue(v == 10, "mutList01 failed: expected pop == Some(10).")
            case None => bug!("mutList01 failed: expected Some.")
        };
        assertTrue(MutList.isEmpty(l), "mutList01 failed: expected empty after pops.")
    }

    @Test
    def mutDeque01(): Unit = region rc {
        let d = MutDeque.empty(rc);
        MutDeque.pushBack(1, d);
        MutDeque.pushBack(2, d);
        MutDeque.pushFront(0, d);
        let _ = assertTrue(MutDeque.size(d) == 3, "mutDeque01 failed: expected size == 3.");
        match MutDeque.popFront(d) {
            case Some(x) => assertTrue(x == 0, "mutDeque01 failed: expected popFront == 0.")
            case None => bug!("mutDeque01 failed: expected Some.")
        };
        match MutDeque.popBack(d) {
            case Some(x) => assertTrue(x == 2, "mutDeque01 failed: expected popBack == 2.")
            case None => bug!("mutDeque01 failed: expected Some.")
        };
        match MutDeque.popFront(d) {
            case Some(x) => assertTrue(x == 1, "mutDeque01 failed: expected remaining element == 1.")
            case None => bug!("mutDeque01 failed: expected Some.")
        };
        assertTrue(MutDeque.isEmpty(d), "mutDeque01 failed: expected empty after pops.")
    }

    @Test
    def mutHashMap01(): Unit = region rc {
        let m = MutHashMap.empty(rc);
        MutHashMap.put(1, "one", m);
        MutHashMap.put(2, "two", m);
        let _ = assertTrue(MutHashMap.size(m) == 2, "mutHashMap01 failed: expected size == 2.");
        match MutHashMap.get(1, m) {
            case Some(v) => assertTrue(v == "one", "mutHashMap01 failed: expected get(1) == Some(\"one\").")
            case None => bug!("mutHashMap01 failed: expected Some.")
        };
        match MutHashMap.get(2, m) {
            case Some(v) => assertTrue(v == "two", "mutHashMap01 failed: expected get(2) == Some(\"two\").")
            case None => bug!("mutHashMap01 failed: expected Some.")
        };
        match MutHashMap.get(3, m) {
            case Some(_) => bug!("mutHashMap01 failed: expected get(3) == None.")
            case None => ()
        };
        ()
    }

    @Test
    def mutHashSet01(): Unit = region rc {
        let s = MutHashSet.empty(rc);
        MutHashSet.add(1, s);
        MutHashSet.add(2, s);
        let _ = assertTrue(MutHashSet.size(s) == 2, "mutHashSet01 failed: expected size == 2.");
        let _ = assertTrue(MutHashSet.memberOf(1, s), "mutHashSet01 failed: expected memberOf(1) == true.");
        let _ = assertTrue(MutHashSet.memberOf(2, s), "mutHashSet01 failed: expected memberOf(2) == true.");
        let _ = assertTrue(not MutHashSet.memberOf(3, s), "mutHashSet01 failed: expected memberOf(3) == false.");
        MutHashSet.remove(2, s);
        let _ = assertTrue(MutHashSet.size(s) == 1, "mutHashSet01 failed: expected size == 1 after remove.");
        assertTrue(not MutHashSet.memberOf(2, s), "mutHashSet01 failed: expected memberOf(2) == false after remove.")
    }

    @Test
    def mutMap01(): Unit = region rc {
        let m = MutMap.empty(rc);
        MutMap.put(1, "one", m);
        MutMap.put(2, "two", m);
        let _ = assertTrue(MutMap.size(m) == 2, "mutMap01 failed: expected size == 2.");
        match MutMap.get(1, m) {
            case Some(v) => assertTrue(v == "one", "mutMap01 failed: expected get(1) == Some(\"one\").")
            case None => bug!("mutMap01 failed: expected Some.")
        };
        match MutMap.get(2, m) {
            case Some(v) => assertTrue(v == "two", "mutMap01 failed: expected get(2) == Some(\"two\").")
            case None => bug!("mutMap01 failed: expected Some.")
        };
        match MutMap.get(3, m) {
            case Some(_) => bug!("mutMap01 failed: expected get(3) == None.")
            case None => ()
        };
        ()
    }

    @Test
    def mutSet01(): Unit = region rc {
        let s = MutSet.empty(rc);
        MutSet.add(1, s);
        MutSet.add(2, s);
        let _ = assertTrue(MutSet.size(s) == 2, "mutSet01 failed: expected size == 2.");
        let _ = assertTrue(MutSet.memberOf(1, s), "mutSet01 failed: expected memberOf(1) == true.");
        let _ = assertTrue(MutSet.memberOf(2, s), "mutSet01 failed: expected memberOf(2) == true.");
        let _ = assertTrue(not MutSet.memberOf(3, s), "mutSet01 failed: expected memberOf(3) == false.");
        MutSet.remove(2, s);
        let _ = assertTrue(MutSet.size(s) == 1, "mutSet01 failed: expected size == 1 after remove.");
        assertTrue(not MutSet.memberOf(2, s), "mutSet01 failed: expected memberOf(2) == false after remove.")
    }

    @Test
    def mutPriorityQueue01(): Unit = region rc {
        let q = MutPriorityQueue.empty(rc);
        let _ = assertTrue(MutPriorityQueue.isEmpty(q), "mutPriorityQueue01 failed: expected empty at start.");
        MutPriorityQueue.enqueue(2, q);
        MutPriorityQueue.enqueue(1, q);
        MutPriorityQueue.enqueue(3, q);
        let _ = assertTrue(MutPriorityQueue.size(q) == 3, "mutPriorityQueue01 failed: expected size == 3.");
        match MutPriorityQueue.peek(q) {
            case Some(v) => assertTrue(v == 3, "mutPriorityQueue01 failed: expected peek == Some(3).")
            case None => bug!("mutPriorityQueue01 failed: expected Some.")
        };
        match MutPriorityQueue.dequeue(q) {
            case Some(v) => assertTrue(v == 3, "mutPriorityQueue01 failed: expected dequeue == Some(3).")
            case None => bug!("mutPriorityQueue01 failed: expected Some.")
        };
        match MutPriorityQueue.dequeue(q) {
            case Some(v) => assertTrue(v == 2, "mutPriorityQueue01 failed: expected dequeue == Some(2).")
            case None => bug!("mutPriorityQueue01 failed: expected Some.")
        };
        match MutPriorityQueue.dequeue(q) {
            case Some(v) => assertTrue(v == 1, "mutPriorityQueue01 failed: expected dequeue == Some(1).")
            case None => bug!("mutPriorityQueue01 failed: expected Some.")
        };
        match MutPriorityQueue.dequeue(q) {
            case Some(_) => bug!("mutPriorityQueue01 failed: expected dequeue == None on empty queue.")
            case None => ()
        };
        ()
    }

    @Test
    def mutDisjointSets01(): Unit = region rc {
        let s = MutDisjointSets.empty(rc);
        let _ = assertTrue(not MutDisjointSets.memberOf(1, s), "mutDisjointSets01 failed: expected memberOf(1) == false at start.");
        MutDisjointSets.makeSet(1, s);
        MutDisjointSets.makeSet(2, s);
        let _ = assertTrue(MutDisjointSets.memberOf(1, s), "mutDisjointSets01 failed: expected memberOf(1) == true after makeSet.");
        let _ = assertTrue(MutDisjointSets.memberOf(2, s), "mutDisjointSets01 failed: expected memberOf(2) == true after makeSet.");
        let _ = assertTrue(not MutDisjointSets.equivalent(1, 2, s), "mutDisjointSets01 failed: expected not equivalent before union.");
        MutDisjointSets.union(1, 2, s);
        let _ = assertTrue(MutDisjointSets.equivalent(1, 2, s), "mutDisjointSets01 failed: expected equivalent after union.");
        match (MutDisjointSets.find(1, s), MutDisjointSets.find(2, s)) {
            case (Some(r1), Some(r2)) => assertTrue(r1 == r2, "mutDisjointSets01 failed: expected same representative for 1 and 2.")
            case _ => bug!("mutDisjointSets01 failed: expected find to return Some for members.")
        };

        MutDisjointSets.union(3, 4, s);
        let _ = assertTrue(MutDisjointSets.memberOf(3, s), "mutDisjointSets01 failed: expected memberOf(3) == true after union.");
        let _ = assertTrue(MutDisjointSets.memberOf(4, s), "mutDisjointSets01 failed: expected memberOf(4) == true after union.");
        assertTrue(MutDisjointSets.equivalent(3, 4, s), "mutDisjointSets01 failed: expected equivalent(3,4) == true after union.")
    }

    pub def runAll(): Unit = {
        let _ = mutList01();
        let _ = mutDeque01();
        let _ = mutHashMap01();
        let _ = mutHashSet01();
        let _ = mutMap01();
        let _ = mutSet01();
        let _ = mutPriorityQueue01();
        let _ = mutDisjointSets01();
        ()
    }

}
