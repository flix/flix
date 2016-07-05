//
// Welcome to the Flix delta debugging tutorial!
//

//
// Delta debugging is a fault localization techbnique that attempts to find the
// minimal input that triggers a specific bug. In the context of Flix, this
// means to find the smallest set of facts that triggers a specific exception.
//

//
// Delta debugging is an iterative technique that repeatedly tries to shrink
// the set of input facts. Flix uses a greedy heuristic which means that it
// may not find THE smallest set of facts. On the upside, we can stop delta
// debugging at any time and we will have a smaller input.
//

namespace Tutorial.Delta {

    //
    // Let us begin with a very simple example.
    //

    //
    // Assume we have one relation A:
    //
    rel A(x: Int)

    //
    // This relation has 10 facts:
    //
    A(0). A(1). A(2). A(3). A(4). A(5). A(6). A(7). A(8). A(9).

    //
    // We now add an integrity rule. This rule is special becase it has
    // false in its head. Intuitively, the rule means that if the body
    // can be satisfied then there is an error.
    //
    // Let us assume that if we ever see the fact A(7) that is an error.
    //
    // Now outcomment the line below:
    //
    false :- A(7).

    //
    // We can run this program with:
    //
    // java -jar flix.jar <tutorial-file>
    //
    // which should produce a result like:
    //
    //     Integrity rule violated delta.flix:43:5
    //
    //     43|    false :- A(7).
    //            ^^^^^
    //
    // OK, good. Flix sees the error.
    //

    //
    // We have ten input facts A(0) ... A(9).
    //
    // We can use delta debugging to reduce this to a smaller set of
    // input facts that will trigger the same error. To do so, run:
    //
    // java -jar flix.jar --delta out.flix <tutorial-file>
    //
    // The output should look something like:
    //
    //   Caught `ca.uwaterloo.flix.api.RuleException' with message:
    //       `The integrity rule defined at delta-debugging.flix:43:5 is violated.'
    //   Delta Debugging Started. Trying to minimize 10 facts.
    //
    // After each iteration, the current set of facts is written to `out.flix`.
    //
    // You can inspect this file as the delta debugger is running or when it
    // has completed. In this case it runs till completion, and we see:
    //
    //   >>> Delta Debugging Complete! <<<
    //   >>> Output written to `out.flix'. <<<
    //
    // And when we open `out.flix`:
    //
    //   Tutorial.Delta/A(7i32).
    //
    // which is what we expected.


}