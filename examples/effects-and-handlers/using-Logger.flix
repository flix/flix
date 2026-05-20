def main(): Unit \ IO =
    run {
        Logger.info("Hello");
        Logger.warn("World")
    } with Logger.runWithIO
