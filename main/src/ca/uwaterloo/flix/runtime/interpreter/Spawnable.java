package ca.uwaterloo.flix.runtime.interpreter;

// TODO delete this and use Runnable + gen the below code inline in genexpression
public interface Spawnable {
    // TODO delete this
    static void spawn(Runnable s) {
        // Create a new Thread and evaluate the spawned expression in the new Thread
        Thread thread = new Thread(s);
        thread.start();
    }
}
