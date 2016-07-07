//
// Welcome to the delta debugging tutorial!
//

//
// Delta debugging is a general-purpose fault localization technique that
// attempts to find the minimal input that triggers a specific bug in a program.
//
// In the context of Flix this means to find the minimal set of input facts
// that triggers a specific exception, whether that be a rule integrity
// violation, a match/switch exception, user-specified exception, or even an
// uncaught exception in the Flix solver itself.
//
// Delta debugging iteratively tries to shrink the set of input facts using
// a greedy divide-and-conquer strategy. The result produced by each
// iteration is guaranteed to trigger the specific bug. This means that the
// delta debugger can be stopped at any time. For example, if you get tired
// of waiting :)
//
// Note 1: The greedy strategy is *NOT* guaranteed to find THE minimal solution.
// Note 2: The delta debugger will eventually terminate when it cannot
// remove any more facts without also preventing the error from occurring.
//

namespace Tutorial.DeltaDebugging {

    //
    // Assume we have three relations A, B, and C:
    //
    rel A(x: Int)
    rel B(x: Int)
    rel C(x: Int)

    //
    // And a whole bunch of facts:
    //
    A(0). A(1). A(2). A(3). A(4). A(5). A(6). A(7). A(8). A(9).
    B(0). B(1). B(2). B(3). B(4). B(6). B(5). B(7). B(8). B(9).
    C(0). C(1). C(2). C(3). C(4). C(5). C(6). C(7). C(8). C(9).

    //
    // We know, due to some domain knowledge, that we should never have that
    // A(3), B(5) and C(7) are true at the same time. We can express that
    // with what is called an integrity rule:
    false :- A(3), B(5), C(7).

    //
    // The false value, which appears in the head of this rule, tells Flix to
    // throw a `RuleException` if the body of the rule is ever true.
    //

    //
    // We can run this program with:
    //
    //  $ java -jar flix.jar delta-debugging.flix
    //
    // which should produce a result like:
    //
    //     Integrity rule violated delta.flix:45:5
    //
    //     45|    false :-  A(3), B(5), C(7).
    //            ^^^^^

    //
    // OK, good? Flix should see the error.
    //

    //
    // Try to remove the fact B(5), run the program again and observe that the
    // integrity rule is no longer violated (no exception is thrown).
    //

    //
    // We have thirty input facts A(0) ... A(9), B(0) ... B(9). and C(0) ... C(9).
    //
    // We can use delta debugging to reduce this to a smaller set of input facts
    // that will still trigger the same exception. To do so, run:
    //
    //  $ java -jar flix.jar --delta out.flix delta-debugging.flix
    //
    // The output should look something like:
    //
    //   Caught `ca.uwaterloo.flix.api.RuleException' with message:
    //       `The integrity rule defined at delta-debugging.flix:45:5 is violated.'
    //   Delta Debugging Started. Trying to minimize 30 facts.
    //
    // After each iteration, the current set of facts is written to `out.flix`.
    //
    // You can inspect this file as the delta debugger is running.
    //
    // Open `out.flix` and you should see:
    //
    //   Tutorial.DeltaDebugging/A(3i32).
    //   Tutorial.DeltaDebugging/B(5i32).
    //   Tutorial.DeltaDebugging/C(7i32).
    //
    // which is what we expected.
    //

}