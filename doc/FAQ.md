# Questions and Answers #
The purpose of this document is to provide succinct answers to some of the
questions about the Flix programming language.

### What is Flix? ###
Flix is logic and functional programming languages for fixpoint programming.

Flix is by design *not* a general-purpose programming language.
Flix is *not* Turing-complete.

() Why (when) should I use Flix?

### What is a Flix program? ###
A Flix program is a




(2) What are the components of a Flix program?




() Does Flix support negation?
  Short answer: No. In theory it seems that Flix could support a form of stratified negation, but this a subject for future research.

() Is Flix decidable?

(4) Why are Flix functions required to be strict?

(5) Why are Flix functions required to be monotone?

(6) Why are function symbols disallowed on in the body of a rule?

() Why are Flix functions required to be simply-typed?

(7) What guarantees are provided by Flix?

(8) How are Flix programs evaluated?

() What constitutes a lattice?

() What are the properties of a Flix program?

() What language constructs are supported?

() Does Flix support modules, data abstraction, polymorphism, ...?

() How does Flix prove safety properties?


## Comparison to Other Languages and Tools #

### What is the relationship between Flix and SQL? ###
SQL -- *structured query language* -- is, as the name implies, a language
for querying relational databases. The mathematical foundation for SQL is
relational algebra which define a collection of operators which act on sets
of tuples. In SQL (and relational algebra) a collection of *tables* contain
*rows* which can be extracted and manipulated using the relational algebra
operators. 


### What is the relationship between Flix and Datalog? ###

### What is the relationship between Flix and constraint logic programming? ###

### What is the relationship between Flix and Prolog? ###

### What is the relationship between Flix and PAG? ###

### What is the relationship between Flix and WALA? ###

### What is the relationship between Flix and IFDS/IDE? ###

### What is the relationship between Flix and HOPL? ###

### What is the relationship between Flix and DOOP? ###

### What is the relationship between Flix and SOOT? ###

### What is the relationship between Flix and SAT/SMT solvers? ###


## More Information ##

### Where can I get more information? ###


### Where can I report a bug? ###
Flix is hosted on GitHub and uses the associated bug tracker.

## Additional Information ##
Detailed information about the design and development of Flix is available in
the `DesignChoices.md` document.
