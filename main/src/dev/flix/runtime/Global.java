package dev.flix.runtime;

import java.net.ServerSocket;
import java.net.Socket;

/**
 * OBS: A interface-like copy of the class generated in
 * `main/src/ca/uwaterloo/flix/language/phase/jvm/GenGlobalClass.scala` with
 * exception bodies that allows the compiler to load the imports in i.e. channel
 * and check for types (the code here is never run).
 */
public final class Global {

    public static final long newId() {
        throw new RuntimeException("Global.newId should not be called on the mock class");
    }

    public static final String[] getArgs() {
        throw new RuntimeException("Global.getArgs should not be called on the mock class");
    }

    public static final void setArgs(String[] var0) {
        throw new RuntimeException("Global.setArgs should not be called on the mock class");
    }

    // --- TCP sockets ---

    public static final void putTcpSocket(long id, Socket socket) {
        throw new RuntimeException("Global.putTcpSocket should not be called on the mock class");
    }

    public static final Socket getTcpSocket(long id) {
        throw new RuntimeException("Global.getTcpSocket should not be called on the mock class");
    }

    public static final Socket removeTcpSocket(long id) {
        throw new RuntimeException("Global.removeTcpSocket should not be called on the mock class");
    }

    // --- TCP servers ---

    public static final void putTcpServer(long id, ServerSocket server) {
        throw new RuntimeException("Global.putTcpServer should not be called on the mock class");
    }

    public static final ServerSocket getTcpServer(long id) {
        throw new RuntimeException("Global.getTcpServer should not be called on the mock class");
    }

    public static final ServerSocket removeTcpServer(long id) {
        throw new RuntimeException("Global.removeTcpServer should not be called on the mock class");
    }

    // --- Processes ---

    public static final void putProcess(long id, Process process) {
        throw new RuntimeException("Global.putProcess should not be called on the mock class");
    }

    public static final Process getProcess(long id) {
        throw new RuntimeException("Global.getProcess should not be called on the mock class");
    }

    public static final Process removeProcess(long id) {
        throw new RuntimeException("Global.removeProcess should not be called on the mock class");
    }

}
