// Order of difficulty

eff Console {
    //

}


eff Random1 {

}

mod Random1 {
    pub def randomBool(): Unit = ???
}



eff Logger {
    pub def info(s: String): Unit
}

enum Socket

eff SocketIO {
    pub def connect(host: String, port: Int32): Unit

    pub def getInputStream(s: Socket): Unit

    // lots of getters and setters.

    pub def close(s: Socket): Unit
}

eff Thread {

}

// TODO: Env like in ZIO

eff InputStream {

}

eff Environment {

}

eff Reader {

}

eff BufferedReader {

}

eff Time {

}
