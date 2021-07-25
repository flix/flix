package ca.uwaterloo.flix.runtime.interpreter;

import java.util.*;
import java.util.concurrent.ThreadLocalRandom;
import java.util.concurrent.atomic.AtomicInteger;
import java.util.concurrent.locks.*;


public final class Spawner {

  public static void spawn(Spawnable s) {
    // Create a new Thread and evaluate the spawned expression in the new Thread
    Thread thread = new Thread(s::spawn);
    thread.start();
  }

}
