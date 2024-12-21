def main(): Unit \ IO =
    region rc {
        spawn (println("Hello")) @ rc;
        spawn (println("Word!")) @ rc
    }
