use Assert.{assertEq, runWithLogger}

/// Demonstrates the `runWithLogger` handler which routes assertion failures
/// to the `Logger` effect. The outer `Logger.runWithIO` handler prints the log messages.
def main(): Unit \ IO =
    run {
        assertEq(expected = 42, 21 + 21);
        assertEq(expected = 10, 3 + 3);
        println("Execution continued after failing assertion.")
    } with runWithLogger
      with Logger.runWithIO
