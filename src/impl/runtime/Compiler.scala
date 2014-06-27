package impl.runtime

import impl.{Term, Program}

import scala.util.Try

class Compiler(p: Program) {

  def verify(): Try[Any] = Try({
    // 1. Each fact must be ground.
    p.facts.find(!_.isGround).exists(f => throw new RuntimeException(s"The fact $f is not ground!"))

    // 2. Each variable which occurs in the head of a rule of must also occur in its body.
    for (rule <- p.rules) {
      for (term <- rule.head.terms) {
        term match {
          case Term.Variable(v) => if (!rule.body.exists(predicate => predicate.terms.contains(Term.Variable(v))))
            throw new RuntimeException();
          case Term.Constant(c) => // nop
          case _ => ???
        }
      }
    }

    ???
  })

}


