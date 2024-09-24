package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.tools.pkg.Dependency.JarDependency
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.{IOException, InputStream, PrintStream}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.util.Using

object JarPackageManager {

  val FolderName = "external"

  /**
    * Installs all the jar dependencies for a list of Manifests at the /lib/external folder
    * of `path` and returns a list of paths to all the dependencies.
    */
  def installAll(manifests: List[Manifest], path: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Downloading external jar dependencies...")

    val allJarDeps: List[JarDependency] = manifests.foldLeft(List.empty[JarDependency])((l, m) => l ++ findJarDependencies(m))

    val jarPaths = allJarDeps.map(dep => {
      install(dep, path) match {
        case Ok(p) => p
        case Err(e) => out.println(s"ERROR: Installation of `${dep.fileName}` from `${dep.url.toString}` failed."); return Err(e)
      }
    })

    Ok(jarPaths)
  }

  /**
    * Installs a jar file from a URL given by `dep.url`.
    *
    * The file is installed at lib/external/`dep.fileName`.
    *
    * Returns the path to the downloaded file.
    */
  private def install(dep: JarDependency, p: Path)(implicit out: PrintStream): Result[Path, PackageError] = {
    val lib = Bootstrap.getLibraryDirectory(p)
    val folderPath = lib.resolve(FolderName)

    //create the folder if it does not exist
    Files.createDirectories(folderPath)
    val assetPath = folderPath.resolve(dep.fileName)

    if (Files.exists(assetPath)) {
      out.println(s"  Cached `${dep.fileName}` from `${dep.url.toString}`.")
      Ok(assetPath)
    } else {
      out.print(s"  Downloading `${dep.fileName}` from `${dep.url.toString}`... ")
      out.flush()
      try {
        Using(dep.url.openStream()) {
          stream => Files.copy(stream, assetPath, StandardCopyOption.REPLACE_EXISTING)
        }
      } catch {
        case e: IOException =>
          out.println(s"ERROR: ${e.getMessage}.");
          return Err(PackageError.DownloadErrorJar(dep.url.toString, dep.fileName, Some(e.getMessage)))
      }
      if (Files.exists(assetPath)) {
        out.println(s"OK.")
        Ok(assetPath)
      } else {
        out.println(s"ERROR.")
        return Err(PackageError.DownloadErrorJar(dep.url.toString, dep.fileName, None))
      }
    }
    Ok(assetPath)
  }

  /** Finds the jar dependencies in a Manifest. */
  private def findJarDependencies(manifest: Manifest): List[JarDependency] = {
    manifest.dependencies.collect { case dep: JarDependency => dep }
  }

}
