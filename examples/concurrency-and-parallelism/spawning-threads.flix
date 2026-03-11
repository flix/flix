def main(): Unit \ IO =
    region rc {
        spawn (println("Hello")) @ rc;
        spawn (println("World")) @ rc
    }
