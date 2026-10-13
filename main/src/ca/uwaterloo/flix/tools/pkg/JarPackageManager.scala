package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.tools.pkg.Dependency.JarDependency
import ca.uwaterloo.flix.tools.pkg.github.GitHub
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.PrintStream
import java.nio.file.{Files, Path}

object JarPackageManager {

  val DirName = "external"

  /**
    * Installs all the jar dependencies for a list of Manifests at the /lib/external directory
    * of `path` and returns a list of paths to all the dependencies.
    */
  def installAll(manifests: List[Manifest], path: Path, token: Option[String])(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Downloading external jar dependencies...")

    val allJarDeps: List[JarDependency] = manifests.foldLeft(List.empty[JarDependency])((l, m) => l ++ findJarDependencies(m))

    val jarPaths = allJarDeps.map(dep => {
      install(dep, path, token) match {
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
    * The address is the dependent's to choose and may be anyone's to serve, so the request is
    * made the way every other one is: redirects followed, the status read, and `token` offered
    * only if the address is one it may be sent to. A jar that is published as a GitHub release
    * asset is therefore fetched with the token, and one served from anywhere else without it.
    *
    * Returns the path to the downloaded file.
    */
  private def install(dep: JarDependency, p: Path, token: Option[String])(implicit out: PrintStream): Result[Path, PackageError] = {
    val lib = Bootstrap.getLibraryDirectory(p)
    val dirPath = lib.resolve(DirName)

    //create the directory if it does not exist
    Files.createDirectories(dirPath)
    val assetPath = dirPath.resolve(dep.fileName)

    if (Files.exists(assetPath)) {
      out.println(s"  Cached `${dep.fileName}` from `${dep.url.toString}`.")
      Ok(assetPath)
    } else {
      out.print(s"  Downloading `${dep.fileName}` from `${dep.url.toString}`... ")
      out.flush()
      GitHub.download(dep.getUrl, assetPath, token) match {
        case Ok(()) =>
          out.println("OK.")
          Ok(assetPath)

        case Err(e) =>
          out.println("ERROR.")
          Err(e)
      }
    }
  }

  /**
    * Finds the jar dependencies in a Manifest.
    */
  private def findJarDependencies(manifest: Manifest): List[JarDependency] = {
    manifest.dependencies.collect { case dep: JarDependency => dep }
  }

}
