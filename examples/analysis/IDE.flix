namespace IDE {

    enum Value {
        case Top,
        case Cst(Int),
        case Bot
    }

    def equ(e1: Value, e2: Value): Bool = e1 == e2

    def lub(e1: Value, e2: Value): Value = match (e1, e2) {
        case (Value.Bot, x) => x
        case (x, Value.Bot) => x
        case (Value.Cst(n1), Value.Cst(n2)) => if(n1 == n2) Value.Cst(n1) else Value.Top
        case _ => Value.Top
    }

    def leq(e1: Value, e2: Value): Bool = match (e1, e2) {
        case (Value.Bot, _) => true
        case (Value.Cst(n1), Value.Cst(n2)) => n1 == n2
        case (_, Value.Top) => true
        case _ => false
    }

    def sum(e1: Value, e2: Int): Value = match e1 {
        case Value.Cst(n) => Value.Cst(n+e2)
        case _ => e1
    }

    def prod(e1: Value, e2: Int): Value = match (e1, e2) {
        case (Value.Cst(n), _) => Value.Cst(n*e2)
        case (Value.Bot, _) => Value.Bot
        case (_, zero) => if(zero == 0) Value.Cst(0) else e1
        case _ => e1
    }

    def glb(e1: Value, e2: Value): Value = match (e1, e2) {
        case (Value.Top, x) => x
        case (x, Value.Top) => x
        case (Value.Cst(n1), Value.Cst(n2)) => if(n1 == n2) Value.Cst(n1) else Value.Bot
        case _ => Value.Bot
    }

    let Value<> = (Value.Bot, Value.Top, equ, leq, lub, glb)

    enum Transformer {
        case BotTrans,
        case NonBotTrans(Int,Int,Value)
    }

  def compose(t1: Transformer, t2: Transformer): Transformer = match (t1, t2) {
    case (_,Transformer.BotTrans) => Transformer.BotTrans
    case (Transformer.BotTrans, Transformer.NonBotTrans(_, _, c)) => match c {
      case Value.Bot => Transformer.BotTrans
      case Value.Top => Transformer.NonBotTrans(0,0,Value.Top)
      case Value.Cst(cc) => Transformer.NonBotTrans(0,cc,c)
    }
    case (Transformer.NonBotTrans(a2, b2, c2), Transformer.NonBotTrans(a1, b1, c1)) =>
      Transformer.NonBotTrans(a1*a2, (a1*b2)+b1, lub(sum(prod(c2, a1), b1), c1))
  }

    // from paper: f = \l. a*l+b meet c; f(Top) = Top
    // paper is upside-down, so: f = \l. a*l+b join c; f(Bot) = Bot
    def apply(t: Transformer, l: Value): Value = match t {
        case Transformer.BotTrans => Value.Bot
        case Transformer.NonBotTrans(a, b, c) => if(l==Value.Bot) Value.Bot else lub(sum(prod(l,a),b),c)
    }

    def translub(t1: Transformer, t2: Transformer): Transformer = match (t1, t2) {
        case (Transformer.BotTrans, _) => t2
        case (_, Transformer.BotTrans) => t1
        case (Transformer.NonBotTrans(a1, b1, c1), Transformer.NonBotTrans(a2, b2, c2)) =>
          if (a1 == a2 and b1 == b2)
            Transformer.NonBotTrans(a1, b1, lub(c1, c2))
         else if(c1 == c2)
             if(0==(b1-b2)%(a2-a1))
        //                // is divisible
               Transformer.NonBotTrans(a1, b2, lub(Value.Cst(a1*(b1-b2)/(a2-a1)+b1), lub(c1, c2)))
        //                // is not divisible
               else Transformer.NonBotTrans(1, 0, Value.Top)
         else ???: Transformer
    }

    def transequ(e1: Transformer, e2: Transformer): Bool = e1 == e2

    def transleq(e1: Transformer, e2: Transformer): Bool = e2 == translub(e1, e2)

    def transglb(_: Transformer, _: Transformer): Transformer = ???: Transformer

    let Transformer<> = (Transformer.BotTrans, ???: Transformer, transequ, transleq, translub, transglb)

    // Inputs
    lat EshIntra(node: String, infact: String, outfact: String, transformer: Transformer)
    lat EshCallStart(node: String, infact: String, proc: String, outfact: String, transformer: Transformer)
    lat EshEndReturn(proc: String, infact: String, node: String, outfact: String, transformer: Transformer)

    // Call-to-Return edges should be included above in EshIntra
    rel CFG(src: String, dst: String)
    rel CallGraph(callsite: String, proc: String)
    rel StartNode(proc: String, node: String)
    rel EndNode(proc: String, node: String)

    // Internal lattices
    lat JumpFn(infact: String, node: String, outfact: String, transformer: Transformer)
    lat SummaryFn(node: String, infact: String, outfact: String, transformer: Transformer)
    lat ResultProc(proc: String, fact: String, value: Value)
    rel InProc(proc: String, node: String)

    // Output
    lat Results(node: String, fact: String, value: Value)

    // Rules for Example (constant propagation) from IDE paper
    def id(): Transformer = Transformer.NonBotTrans(1,0,Value.Bot)

    // call-to-start
    def nonbottom(t: Transformer): Bool = t != Transformer.BotTrans

    // Rules
    @test
    def test(): #{ EshIntra, EshCallStart, EshEndReturn, CFG, CallGraph, StartNode, EndNode, JumpFn, SummaryFn, ResultProc, InProc, Results } = solve #{
        // nodes in a given procedure
        InProc(p,start) :- StartNode(p, start).
        InProc(p,m) :- InProc(p,n), CFG(n,m).

        // intraproc
        JumpFn(d1,m,d3;compose(long,short)) :-
            CFG(n,m),
            JumpFn(d1,n,d2;long),
            EshIntra(n,d2,d3;short).

        // use summary
        JumpFn(d1,m,d3;compose(caller,summary)) :-
            CFG(n,m),
            JumpFn(d1,n,d2;caller),
            SummaryFn(n,d2,d3;summary).

        JumpFn(d3,start,d3;id()) :-
            JumpFn(d1,call,d2;nonbottom1),
            CallGraph(call,target),
            EshCallStart(call,d2,target,d3;nonbottom2),
            StartNode(target,start),
            nonbottom(nonbottom1),
            nonbottom(nonbottom2).

        // compute summary
        SummaryFn(call,d4,d5;compose(compose(cs,se),er)) :-
            CallGraph(call,target),
            StartNode(target,start),
            EndNode(target,end),
            EshCallStart(call,d4,target,d1;cs),
            JumpFn(d1,end,d2;se),
            EshEndReturn(target,d2,call,d5;er).

        // tabulate result
        Results(n, d; apply(fn, vp)) :-
            ResultProc(proc, dp; vp),
            InProc(proc, n),
            JumpFn(dp, n, d; fn).

        ResultProc(proc,dp;apply(cs,v)) :-
            Results(call,d;v),
            EshCallStart(call,d,proc,dp;cs).

        CFG("smain","n1").
        CFG("n1","n2").
        CFG("n2","n3").
        CFG("n3","emain").

        CFG("sp","n4").
        CFG("n4","n5").
        CFG("n4","n9").
        CFG("n5","n6").
        CFG("n6","n7").
        CFG("n7","n8").
        CFG("n8","n9").
        CFG("n9","ep").

        StartNode("main","smain").
        StartNode("p","sp").
        EndNode("main","emain").
        EndNode("p","ep").

        CallGraph("n1","p").
        CallGraph("n6","p").

        EshIntra(n,"zero","zero";id()) :- CFG(n,_).

        EshIntra("smain","zero","x";Transformer.BotTrans).
        EshIntra("n2","x","x";id()).
        EshIntra("n3","x","x";id()).

        EshIntra("sp","a","a";id()).
        EshIntra("sp","x","x";id()).
        EshIntra("n4","a","a";id()).
        EshIntra("n4","x","x";id()).
        EshIntra("n5","a","a";Transformer.NonBotTrans(1,-2,Value.Bot)).
        EshIntra("n5","x","x";id()).
        EshIntra("n6","a","a";id()).
        EshIntra("n7","a","a";id()).
        EshIntra("n7","x","x";id()).
        EshIntra("n8","a","a";Transformer.NonBotTrans(1,2,Value.Bot)).
        EshIntra("n8","x","x";id()).
        EshIntra("n9","a","a";id()).
        EshIntra("n9","a","x";Transformer.NonBotTrans(-2,5,Value.Bot)).

        EshCallStart(call,"zero",target,"zero";id()) :- CallGraph(call,target).
        EshEndReturn(target,"zero",call,"zero";id()) :- CallGraph(call,target).

        EshCallStart("n1","zero","p","a";Transformer.NonBotTrans(0,7,Value.Bot)).
        EshCallStart("n1","x","p","x";id()).
        EshEndReturn("p","x","n1","x";id()).

        EshCallStart("n6","a","p","a";id()).
        EshCallStart("n6","x","p","x";id()).
        EshEndReturn("p","x","n6","x";id()).

        // Entrypoint
        JumpFn("zero","smain","zero";id()).
        ResultProc("main","zero";Value.Top).
    }

}
