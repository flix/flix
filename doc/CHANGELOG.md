# Changelog

HEAD:

Version 0.14.0:
- Added support for Visual Studio Code via LSP.
- Allow `use` directly in namespaces.
- Added `default` expression.
- Added `MutList` to standard library.
- Improved type inference w.r.t. kinds.
- Various bug fixes.

Version 0.13.0:
- Significantly improved compilation time.
- Significantly improved the execution time of some Flix programs.
- Added @Time and @Space annotations.
- Added keywords for the logical operators: not, and, or.
- Reintroduce multi-threaded compilation.
- Various minor improvements to the standard library.
- Various minor bug fixes to the compiler.
- Removed binary literals from the language.

Version 0.12.0:

- Sets and Maps are now backed by Red-Black Trees. 
- Added mutable sets and maps (backed by immutable sets and maps for now).
- Added NonEmptyList (Nel) and Validation.
- Added spaceship operator.
- Added binary, octal, and hex literals.
- Added support for underscores in number literals.
- Added use construct to import defs/types/tags from other namespaces.
- Added a check for poorly named type variables.
- Reworked relation and lattice types. They now work more like type aliases.
- Improved the performance of the compiler.
- Extended the standard library.

Version 0.11.0:

- Added light-weight polymorphic effects.
- Defined division by zero to yield zero.
- Added check for unconditional recursion.
- Reworked array length from `length[x]` to `x.length`.
- Removed switch expression.

Version 0.10.0:

- Reworked and improved Java interoperability.
- Added support for folding on Datalog constraints.

Version 0.9.1:

- Fixed several bugs. 

Version 0.9.0:

- Added support for opaque types.
- Added support for type aliases.
- Added support for pattern matching on arrays.
- Added experimental support for monadic let (let*).
- Fixed a few bugs.

Version 0.8.1:

- Fixed several bugs. 

Version 0.8:

- Added Array, Channel, Random, and Path to the standard library.
- Rewrote --doc to output JSON.
- Accurately report negative cycles in the presence of stratified negation.

Version 0.7:

- Changed the syntax of constraint sets.
- Changed syntax for sets and maps.
- Improvements to tree shaking.
- Check for dead and unreachable code.
- Improved stratification of first-class constraints based on types.
- Multiple bug fixes.
- Upgrade to Scala 2.13
- Added support for Gradle builds.

Version 0.6:
- Added preliminary support for a simple build system.

Version 0.5:
- Added support for unbuffered channels.
- Added support for extensible records.

Version 0.4:
- Added support for first-class Datalog constraints.
- Added support for processes and channels.
- Many other improvements.

Version 0.3:
- Added completely new backend with full tail call support (thanks Ramin Zarifi)
- Added support for arrays and vectors.
- Added pub keyword.
- Added expression holes.
- Added full support for curried definitions.
- Vastly improved repl. (--interactive).

Version 0.2:
- Added stratified negation (thanks Jason Mittertreiner).
- Pattern matching now compiled to labels and jumps (~1.5x speedup).
- Typechecking is now performed in parallel.
- Parsing is now performed in parallel.
- Added operations on character and strings.
- Pattern match exhaustiveness enforced.
- Added interop from Flix to Java.
- Added simple intra-procedural optimiser.
- Constructors (tags) may now be used as functions.
- Added references (ref and deref).
- Upgrade to Scala 2.12.
- Many other small improvements.

Version 0.1:
- Added standard library.
- Added support for pattern matching in rules.
- Added if guards.
- Improved error messages.
- Improved the performance of the solver.
- Fixed numerous bugs.

2016-10-25:
- Added interpreter tutorial.
- Added polymorphic types.
- Added unification-based type system.
- Added quick checker.
- Added tail recursion.

2016-07-14:
- Added delta debugging (see --delta and --tutorial delta-debugging).
- Added parallel rule evaluation. (Use --threads to control the number of concurrent threads).
- Added tutorial. (see --tutorial introduction).
- Added --timeout parameter.
- Improved command line parsing. Added a --help flag to display available options.

2016-06-08: 
- Initial release.
