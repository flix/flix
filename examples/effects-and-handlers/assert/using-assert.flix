use Assert.{assertTrue, assertFalse, assertEq, assertNeq}

/// Demonstrates basic assertions using the default handler.
/// The `Assert` effect has a default handler, so no explicit `runWithIO` is needed.
/// All assertions here pass, so the program completes without error.
def main(): Unit \ { Assert, IO } =
    assertTrue(1 + 1 == 2);
    assertFalse(1 > 2);
    assertEq(expected = 4, 2 + 2);
    assertNeq(unexpected = 0, 1 + 1);
    println("All assertions passed!")
