package ca.uwaterloo.flix.tools.pkg

import ca.uwaterloo.flix.api.Bootstrap
import ca.uwaterloo.flix.tools.pkg.Dependency.JarDependency
import ca.uwaterloo.flix.util.Result
import ca.uwaterloo.flix.util.Result.{Err, Ok}

import java.io.{IOException, InputStream, PrintStream}
import java.net.{MalformedURLException, URL}
import java.nio.file.{Files, Path, StandardCopyOption}

object JarPackageManager {

  def installAll(manifests: List[Manifest], path: Path)(implicit out: PrintStream): Result[List[Path], PackageError] = {
    out.println("Downloading jar dependencies...")

    val allJarDeps: List[JarDependency] = manifests.foldLeft(List.empty[JarDependency])((l, m) => l ++ findJarDependencies(m))

    val jarPaths = allJarDeps.map(dep => {
      install(dep, path) match {
        case Ok(p) => p
        case Err(e) => out.println(s"ERROR: Installation of `${dep.fileNameSave}` from `${dep.url}` failed."); return Err(e)
      }
    })

    Ok(jarPaths)
  }

  private def install(dep: JarDependency, p: Path)(implicit out: PrintStream): Result[Path, PackageError] = {
    val lib = Bootstrap.getLibraryDirectory(p)
    val folderPath = lib.resolve(dep.website)
    //create the folder if it does not exist
    Files.createDirectories(folderPath)
    val assetPath = folderPath.resolve(dep.fileNameSave)

    if (Files.exists(assetPath)) {
      out.println(s"  Cached `${dep.fileNameSave}` from `${dep.url}`.")
      Ok(assetPath)
    } else {
      out.print(s"  Downloading `${dep.fileNameSave}` from `${dep.url}`... ")
      out.flush()
      try {
        val url: URL = new URL(s"https://${dep.url}")
        val stream: InputStream = url.openStream()
        Files.copy(stream, assetPath, StandardCopyOption.REPLACE_EXISTING)
      } catch {
        case e: MalformedURLException => println(e.getMessage); ???
        case e: IOException =>
          out.println(s"ERROR: ${e.getMessage}.");
          return Err(PackageError.DownloadError(???, Some(e.getMessage)))
      }
      if (Files.exists(assetPath)) {
        out.println(s"OK.")
        Ok(assetPath)
      } else {
        out.println(s"ERROR.")
        Err(PackageError.DownloadError(???, None))
      }
    }
    Ok(assetPath)
  }

  private def findJarDependencies(manifest: Manifest): List[JarDependency] = {
    manifest.dependencies.collect { case dep: JarDependency => dep }
  }

}
