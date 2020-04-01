// Create a new package. Everything but the name of the package can be populated with sensible defaults from the environment
pub def initPackage(name: String): /* Package */ Result[Path, ErrorFull] & Impure = {
    let getProjectDir = (base) -> Path.add(base, name);

    // Project directories
    let getBuildDir = (base) -> Path.add(getProjectDir(base), "build");
    let getSourceDir = (base) -> Path.add(getProjectDir(base), "src");
    let getTestDir = (base) -> Path.add(getProjectDir(base), "test");

    // Top-level files
    let getPackageFile = (base) -> Path.add(getProjectDir(base), "package.sn");
    let getHistoryFile = (base) -> Path.add(getProjectDir(base), "HISTORY.md");
    let getLicenseFile = (base) -> Path.add(getProjectDir(base), "LICENSE.md");
    let getReadmeFile = (base) -> Path.add(getProjectDir(base), "README.md");

    // Source files
    let getMainSourceFile = (base) -> Path.add(getSourceDir(base), "Main.flix");
    let getMainTestFile = (base) -> Path.add(getTestDir(base), "TestMain.flix");

    let newFile = (content) -> (path) -> {
        Path.writeLines(path, Array.toList(content))
    };

    let workingDir = Path.workingDirectory();

    let files = Array.toList([getPackageFile, getHistoryFile, getLicenseFile, getReadmeFile, getMainSourceFile, getMainTestFile]);
    let dirs = Array.toList([getBuildDir, getSourceDir, getTestDir]);

    // Ensure that none of the paths the init process creates are currently present on the system
    let pathExists = [files, dirs]
        |> Array.toList
        |> List.flatten
        |> List.map(f -> {
                let dir = Result.map(f, workingDir);
                match Result.flatMap(Path.exists, dir) {
                    case Ok(exists) => exists,
                    case _ => true
                }
            })
        |> List.find(x -> x);

    if (Option.isEmpty(pathExists)) {
        // Create all directories required for the package
        List.map(f -> {
            let dir = Result.map(f, workingDir);
            Result.flatMap(Path.mkdirs, dir)
        }, dirs);

        let quote = (s) -> Char.toString('"') + s + Char.toString('"');

        // TODO: Abstract this
        let newPackageFile = newFile([
            "(package",
            "  (name " + name + ")",
            "  (version " + quote("0.1.0") + ")",
            // Empty list/empty value?
            "  (authors ((name " + quote("Rasmus Larsen") + ") (email " + quote("doctorherbstein@gmail.com") + ")))",
            // Should description be quoted?
            "  (description " + quote(name) + ")",
         // "  (license GPLv3)",
         // "  (license (name LGPLv2)",
            "  (license (file " + quote("LICENSE") + ")",
            "  (depends (other_package " + quote("1.2.3") + "))",
            "  (home " + quote("https://github.com/Herbstein/package_one") + ")",
            ")"
        ]);
        Result.flatMap(newPackageFile, Result.map(getPackageFile, workingDir));

        let newHistoryFile = newFile(["### v0.1.0", "  Initial Release"]);
        Result.flatMap(newHistoryFile, Result.map(getHistoryFile, workingDir));

        let newLicenseFile = newFile(["<<ENTER LICENSE INFORMATION>>"]);
        Result.flatMap(newLicenseFile, Result.map(getLicenseFile, workingDir));

        let newReadmeFile = newFile(["# " + name, "", "Enter some useful information", ""]);
        Result.flatMap(newReadmeFile, Result.map(getReadmeFile, workingDir));

        let newMainSourceFile = newFile(["// The main entry point.", "def main(): Unit & Impure = Console.printLine(" + quote("Hello, World") + ")"]);
        Result.flatMap(newMainSourceFile, Result.map(getMainSourceFile, workingDir));

        let newMainTestFile = newFile(["@test", "def testMain01(): Bool & Impure = main() == ()"]);
        Result.flatMap(newMainTestFile, Result.map(getMainTestFile, workingDir));

        // Output something that can be distinguished easily
        Result.mapErr(_ -> GenericIoError, Result.map(getSourceDir, workingDir))
    } else {
        Err(GenericIoError)
    }
}

pub enum ErrorFull {
    case ProjectDirExists
    case GenericIoError
}

//pub def main(): Result[Path, ErrorFull] & Impure = initPackage("package_test")

pub def semver_main(): Bool = match Parser.runParser(SemVer/Parser.parseSemVer(), "1.2.3") {
    case Some(_) => true
    case None => false
}

pub def main(): Bool & Impure = SemVer/Test.allTests()
