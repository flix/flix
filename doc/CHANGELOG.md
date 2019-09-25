# Changelog

HEAD:

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
