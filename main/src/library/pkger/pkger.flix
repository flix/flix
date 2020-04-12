pub def main(): Result[Path, Manifest.InitError] & Impure = Manifest.initPackage("package_test")

pub def semver_main(): Bool = match Parser.runParser(SemVer/Parser.parseSemVer(), "1.2.3") {
    case Some(_) => true
    case None => false
}

// pub def main(): Bool & Impure = Test.runAllTests()
