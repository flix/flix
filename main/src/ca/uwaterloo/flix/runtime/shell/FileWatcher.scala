/*
 * Copyright 2025 Magnus Madsen
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *   http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package ca.uwaterloo.flix.runtime.shell

import java.nio.file.*
import java.nio.file.attribute.BasicFileAttributes
import java.util.concurrent.ConcurrentLinkedQueue
import scala.jdk.CollectionConverters.*

/**
  * A file system watcher that monitors directories for file changes.
  *
  * Uses `java.nio.file.WatchService` to detect file creation, modification, and deletion.
  * Events are accumulated in a thread-safe queue and can be retrieved via `drain()`.
  */
object FileWatcher {

  /**
    * A file system change event.
    */
  sealed trait WatchEvent

  object WatchEvent {
    /** A new file was created. */
    case class Created(path: Path) extends WatchEvent

    /** An existing file was modified. */
    case class Modified(path: Path) extends WatchEvent

    /** A file was deleted. */
    case class Deleted(path: Path) extends WatchEvent

    /** The OS event buffer overflowed; a full re-scan is needed. */
    case object Overflow extends WatchEvent
  }
}

class FileWatcher {

  import FileWatcher.*

  /**
    * The underlying watch service.
    */
  private val watchService: WatchService = FileSystems.getDefault.newWatchService()

  /**
    * Accumulated file change events.
    */
  private val events: ConcurrentLinkedQueue[WatchEvent] = new ConcurrentLinkedQueue()

  /**
    * Maps registered watch keys back to the directory they watch.
    */
  private val keyToDir: java.util.concurrent.ConcurrentHashMap[WatchKey, Path] =
    new java.util.concurrent.ConcurrentHashMap()

  /**
    * The set of directory roots registered for recursive watching.
    * Used to auto-register newly created subdirectories.
    */
  private val recursiveRoots: java.util.concurrent.ConcurrentHashMap[Path, Boolean] =
    new java.util.concurrent.ConcurrentHashMap()

  /**
    * The background daemon thread.
    */
  private var thread: Option[Thread] = None

  /**
    * Registers the given directory for watching (non-recursive, depth 1 only).
    */
  def watchShallow(dir: Path): Unit = {
    if (Files.isDirectory(dir)) {
      registerDirectory(dir)
    }
  }

  /**
    * Registers the given directory and all its subdirectories for watching.
    * Newly created subdirectories will also be automatically registered.
    *
    * If the directory does not exist yet, it is remembered as a recursive root.
    * When it is later created (detected via a parent watch), it will be
    * automatically registered and its contents emitted as Created events.
    */
  def watchRecursively(dir: Path): Unit = {
    recursiveRoots.put(dir.toAbsolutePath.normalize(), true)
    if (Files.isDirectory(dir)) {
      registerTree(dir)
    }
  }

  /**
    * Starts the background thread that polls for file system events.
    */
  def start(): Unit = {
    val t = new Thread(() => pollLoop(), "flix-file-watcher")
    t.setDaemon(true)
    t.start()
    thread = Some(t)
  }

  /**
    * Stops the watcher and releases resources.
    */
  def stop(): Unit = {
    thread.foreach(_.interrupt())
    thread = None
    watchService.close()
  }

  /**
    * Atomically drains all pending events from the queue.
    *
    * @return the list of events accumulated since the last drain, in order.
    */
  def drain(): List[WatchEvent] = {
    val result = List.newBuilder[WatchEvent]
    var event = events.poll()
    while (event != null) {
      result += event
      event = events.poll()
    }
    result.result()
  }

  /**
    * Registers a single directory with the watch service.
    */
  private def registerDirectory(dir: Path): Unit = {
    try {
      val key = dir.register(watchService,
        StandardWatchEventKinds.ENTRY_CREATE,
        StandardWatchEventKinds.ENTRY_DELETE,
        StandardWatchEventKinds.ENTRY_MODIFY
      )
      keyToDir.put(key, dir)
    } catch {
      case _: ClosedWatchServiceException => // watcher was stopped
      case _: java.io.IOException => // directory may have been deleted
    }
  }

  /**
    * Registers a directory and all subdirectories recursively.
    */
  private def registerTree(root: Path): Unit = {
    if (!Files.isDirectory(root)) return
    try {
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def preVisitDirectory(dir: Path, attrs: BasicFileAttributes): FileVisitResult = {
          registerDirectory(dir)
          FileVisitResult.CONTINUE
        }
      })
    } catch {
      case _: java.io.IOException => // best-effort
    }
  }

  /**
    * Scans a newly created directory tree and emits Created events for all files found.
    * This handles the case where a directory is created with files already inside it
    * (e.g. `mkdir -p src && cp file.flix src/`).
    */
  private def scanNewDirectory(root: Path): Unit = {
    try {
      Files.walkFileTree(root, new SimpleFileVisitor[Path] {
        override def visitFile(file: Path, attrs: BasicFileAttributes): FileVisitResult = {
          events.add(WatchEvent.Created(file))
          FileVisitResult.CONTINUE
        }
      })
    } catch {
      case _: java.io.IOException => // best-effort
    }
  }

  /**
    * Returns `true` if the given path is inside a recursively-watched root.
    */
  private def isRecursive(dir: Path): Boolean = {
    val absDir = dir.toAbsolutePath.normalize()
    recursiveRoots.keys().asScala.exists(root => absDir.startsWith(root))
  }

  /**
    * The main event loop that runs on the background thread.
    */
  private def pollLoop(): Unit = {
    try {
      while (!Thread.currentThread().isInterrupted) {
        val key = watchService.take() // blocks until events available
        val dir = keyToDir.get(key)
        if (dir != null) {
          for (rawEvent <- key.pollEvents().asScala) {
            rawEvent.kind() match {
              case StandardWatchEventKinds.OVERFLOW =>
                // Signal that a full re-scan is needed.
                events.add(WatchEvent.Overflow)

              case kind =>
                val child = dir.resolve(rawEvent.context().asInstanceOf[Path])
                kind match {
                  case StandardWatchEventKinds.ENTRY_CREATE =>
                    if (Files.isDirectory(child)) {
                      val absChild = child.toAbsolutePath.normalize()
                      // Register if this directory is a recursive root (e.g. src/ just created)
                      // or is inside an already-recursive root (e.g. src/foo/).
                      if (recursiveRoots.containsKey(absChild) || isRecursive(dir)) {
                        registerTree(child)
                        // Emit Created events for any files already inside the new directory.
                        scanNewDirectory(child)
                      }
                    }
                    if (Files.isRegularFile(child)) {
                      events.add(WatchEvent.Created(child))
                    }

                  case StandardWatchEventKinds.ENTRY_MODIFY =>
                    if (Files.isRegularFile(child)) {
                      events.add(WatchEvent.Modified(child))
                    }

                  case StandardWatchEventKinds.ENTRY_DELETE =>
                    // We cannot check isRegularFile since the file is already gone.
                    // Filter by extension instead (directories don't have .flix/.fpkg/.jar extensions).
                    events.add(WatchEvent.Deleted(child))

                  case _ => // ignore
                }
            }
          }
        }
        key.reset()
      }
    } catch {
      case _: InterruptedException => // shutdown requested
      case _: ClosedWatchServiceException => // watcher was stopped
    }
  }
}
