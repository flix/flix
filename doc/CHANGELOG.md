# Changelog

HEAD:
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