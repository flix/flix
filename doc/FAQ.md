# Frequently Asked Questions #
This document aims to answer some of the commonly asked questions about Flix.

### What is Flix? ###
Flix is declarative programming language which combines aspects of logic and 
functional programming for expressing fixed point algorithms. Flix is by 
design *not* a general-purpose programming language. As such, Flix is *not* 
Turing-complete.

### Why Flix? ###
Flix was created from the observation that Datalog (a logic programming 
language for relations) has been successfully in the implementation of static
analysis tools. Specifically, points-to analysis. Yet, Datalog has many
limitations which hinders it use for a broader class of static analysis 
algorithms. The purpose of Flix is to extend Datalog to remedy some of its
shortcomings.

### What is a Flix program? ###
A Flix program is a




### What are the components of a Flix program? ##




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

### What are the safety properties required by Flix? ###
Flix aims to guarantee that every legal Flix program has a unique least fixed point which can be computed in a finite
number of steps. 

### How does Flix prove safety properties? ###


## Comparison to Related Languages and Tools #

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

## Other Questions ##

### Where can I report a bug? ###
Flix is hosted on GitHub and uses the associated issue tracker.
