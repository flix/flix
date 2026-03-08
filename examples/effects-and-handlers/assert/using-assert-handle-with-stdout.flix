use Assert.{assertEq, runWithStdOut}

/// Demonstrates the `runWithStdOut` handler which prints assertion failures
/// to stdout but allows execution to continue.
def main(): Unit \ IO =
    run {
        assertEq(expected = 4, 2 + 2);
        assertEq(expected = 10, 3 + 3);
        println("Execution continued after failing assertion.")
    } with runWithStdOut
