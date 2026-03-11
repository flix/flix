struct Node[t, r] {
    mut value: t,
    mut next: Option[Node[t, r]]
}

mod Node {
    pub def last(n: Node[t, r]): Option[t] \ r =
        match n->next {
            case None       => Some(n->value)
            case Some(next) => last(next)
        }
}

def main(): Unit \ IO =
    region rc {
        let n3 = new Node @ rc { value = 3, next = None };
        let n2 = new Node @ rc { value = 2, next = Some(n3) };
        let n1 = new Node @ rc { value = 1, next = Some(n2) };
        println(Node.last(n1))
    }
