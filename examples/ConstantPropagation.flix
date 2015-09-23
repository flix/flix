namespace ConstantProp {
    enum Element {
        case Top,
        case Constant(Int),
        case Bot
    };

    def leq(e1: Element, e2: element): Bool = match (e1, e2) with {
        case (Element.Bot, _) => true;
        case (Element.Constant(n1), Element.Constant(n2)) => n1 <= n2;
        case (_, Element.Top) => true;
        case _ => false;
    };

    def lub(e1: Element, e2: Element): Element = match (e1, e2) with {
        case (Element.Bot, _) => e2;
        case (_, Element.Bot) => e1;
        case (Element.Constant(n1), Element.Constant(n2)) => if (n1 == n2) e1 else Element.Top;
        case _ => Element.Top;
    };

    def sum(e1: Element, e2: Element): Element = match (e1, e2) with {
        case (Element.Bot, _) => Element.Bot;
        case (_, Element.Bot) => Element.Bot;
        case (Element.Constant(n1), Element.Constant(n2)) => Element.Constant(n1 + n2);
        case _ => Element.Top;
    };

    def maybeEven(e: Element): Bool = match e with {
        case Element.Bot => false;
        case Element.Constant(n) => (n % 2) == 0;
        case Element.Top => true;
    };

    //TODO(Magnus): Fix this parser bug
    //lat <Element> (Element.Bot, leq, lub);

    rel A(x: <Element>)
    A(Element.Constant(42)).
    A(Element.Constant(56)).
    //A should be Element.Top
    //A(Element.Top) :- true

    rel B(x: <Element>)
    B(sum(Element.Constant(1), Element.Constant(41))).
    //B(Element.Constant(42)) :- true

    rel C(x: <Element>, y: String)
    C(Element.Constant(2), "even").
    C(Element.Bot, "bot").

    rel EvenCs(y: String)
    EvenCs(y) :- maybeEven(e), C(e, y).

}