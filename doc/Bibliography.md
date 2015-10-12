# Bibliography #

## Design Philosophy ##

- **Out of the Tar Pit**, 
  Ben Moseley and Peter Marks.
  [Paper.](http://www.shaffner.us/cs/papers/tarpit.pdf)
  _This paper argues that accidental complexity arises from state and control, 
  and proposes functional-relational programming as a means to reduce this complexity._

## Foundations ##

- **What You Always Wanted to Know About Datalog (and Never Dared to Ask)**, 
  Stefano Ceri, Georg Gottlob and Letizia Tanca.
  [Paper.](http://ieeexplore.ieee.org/xpls/abs_all.jsp?arnumber=43410)
  _This paper provides a good introduction to Datalog._

- **Dedalus: Datalog in Time and Space**,
  Peter Alvaro1, William R. Marczak1, Neil Conway, Joseph M. Hellerstein, David Maier, and Russell Sears.
  [Paper.](http://db.cs.berkeley.edu/papers/datalog2011-dedalus.pdf)
  _This paper presents an extension of Datalog which incorporates the notion of time.
  This has several applications. For example, it allows facts to be retracted between timestamps._
  
- **Logic and Lattices for Distributed Programming**
  Neil Conway, William Marczak, Peter Alvaro, Joseph M. Hellerstein, and David Maier.
  [Paper.](http://db.cs.berkeley.edu/papers/UCB-lattice-tr.pdf)
  _This paper presents an extension of Datalog / Dedalus / Bloom that adds support for user-defined
  lattices and monotone functions in a distributed setting. 
  User-defined functions and lattices are assumed to be well-behaved._

## Related Languages ##

- **The Eve Programming Language**,
  Chris Granger and others.
  [GitHub.](https://github.com/witheve/Eve)
  _This is an interesting new programming language with roots in Datalog._
  

## Motivation and Inspiration ##

- **Using Datalog for Fast and Easy Program Analysis**,
  Yannis Smaragdakis and Martin Bravenboer.
  [Paper.](http://dl.acm.org/citation.cfm?id=2185939)
  _This paper argues for the use of Datalog for the implementation of sophisticated Points-To analyses._  
  
- **Strictly Declarative Specification of Sophisticated Points-to Analyses**,
  Martin Bravenboer and Yannis Smaragdakis.
  [Paper.](http://dl.acm.org/citation.cfm?id=1640108)
  _This paper argues for the use of Datalog for the implementation of sophisticated Points-To analyses._
  
- **CodeQuest: Scalable Source Code Queries with Datalog**,
  Elnar Hajiyev, Mathieu Verbaere and Oege de Moor.
  [Paper.](http://link.springer.com/chapter/10.1007/11785477_2)
  _This paper argues for the use of Datalog queries to allow programmers to inspect the structure of their programs._

- **PAG - An Efficient Program Analyzer Generator**,
  Florian Martin.
  [Paper.](http://link.springer.com/article/10.1007/s100090050017)

- **Hoopl: A Modular, Reusable Library for Dataflow Analysis and Transformation**,
  Norman Ramsey, Joao Dias and Simon Peyton Jones.
  [Paper.](http://dl.acm.org/citation.cfm?id=1863539)


## Optimization ##

- **The Essence of Compiling with Continuations**,
  Cormac Flanagan, Amr Sabry, Bruce F. Duba and Matthias Felleisen.
  [Paper.](https://www.classes.cs.uchicago.edu/archive/2004/fall/32630-1/papers/p237-flanagan.pdf)
  _This paper introduces an intermediate code representation, called A Normal Form, that is useful for optimization._
  
## Human Error Messages ## 
- **Expressive Diagnostics** in Clang, [URL](http://clang.llvm.org/diagnostics.html)

- **Compiler Errors for Humans** in Elm, [URL](http://elm-lang.org/blog/compiler-errors-for-humans)
