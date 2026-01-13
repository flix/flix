plugins {
    application
    id("scala")
}

application {
    mainClass.set("ca.uwaterloo.flix.Main")
}

repositories {
    mavenCentral()
}

dependencies {
    implementation("org.scala-lang:scala-library:2.13.17!!")

    implementation("org.java-websocket:Java-WebSocket:1.6.0")
    implementation("org.jline:jline:3.30.6")
    implementation("org.json4s:json4s-native_2.13:4.0.7")
    implementation("org.ow2.asm:asm:9.8")
    implementation("com.github.rjeschke:txtmark:0.13")
    implementation("com.github.scopt:scopt_2.13:4.1.0")
    implementation("org.apache.commons:commons-lang3:3.18.0")
    implementation("com.lihaoyi:sourcecode_2.13:0.4.4")

    implementation("io.get-coursier:coursier_2.13:2.1.24")

    // Note: The tomlj library determines the antlr library.
    // We cannot upgrade the antlr library independently.
    implementation("org.tomlj:tomlj:1.1.1")
    implementation("org.antlr:antlr4-runtime:4.11.1")

    // JLine uses a logger. We silence it here.
    implementation("org.slf4j:slf4j-nop:2.0.13")

    // Java implementation of LSP.
    implementation("org.eclipse.lsp4j:org.eclipse.lsp4j:0.24.0")

    testImplementation("org.scalatest:scalatest_2.13:3.2.19")
    testRuntimeOnly("org.scala-lang.modules:scala-xml_2.13:2.1.0")
    testRuntimeOnly("org.scalatestplus:junit-5-10_2.13:3.2.19.0")
}

tasks.withType<ScalaCompile>().configureEach {
    scalaCompileOptions.forkOptions.apply {
        memoryMaximumSize = "1536m"
    }
    scalaCompileOptions.additionalParameters.addAll(
        listOf(
            "-Xfatal-warnings",
            "-Ypatmat-exhaust-depth", "400",
            "-release", "21",
            "-opt:inline:ca.uwaterloo.**",
            "-Xmixin-force-forwarders:false",   // Required for LSP4j
            "-Xsource:3",                       // Scala 3 migration flag
            "-Ytasty-reader"                    // Scala 3 migration flag
        )
    )
    // Configure javac options for .java files compiled by scalac via Zinc
    options.compilerArgs.addAll(listOf("--release", "21"))
}

sourceSets {
    main {
        scala {
            setSrcDirs(listOf("main/src"))
        }

        resources {
            setSrcDirs(listOf("main/src/resources"))
        }
    }
    test {
        scala {
            setSrcDirs(listOf("main/test"))
        }
    }
}

tasks.jar {
    manifest {
        attributes("Main-Class" to "ca.uwaterloo.flix.Main")
    }

    from(files(
        // This line has to come before the next
        configurations.runtimeClasspath.get().map { if (it.isDirectory) it else zipTree(it) },
        configurations.compileClasspath.get().map { if (it.isDirectory) it else zipTree(it) }
    )) {
        exclude("META-INF/*.RSA", "META-INF/*.SF", "META-INF/*.DSA")
    }

    from("main") {
        include("**/*.flix")
        include("**/*.json")
        include("**/*.zip")
        include("**/ClassList.txt")
    }

    duplicatesStrategy = DuplicatesStrategy.EXCLUDE
}

tasks.test {
    useJUnitPlatform {
        includeEngines("scalatest")
    }
    testLogging {
        events("passed", "skipped", "failed", "standard_error")
    }
    filter {
        // We only want to run the TestAll suite. Otherwise ALL tests are run.
        includeTestsMatching("ca.uwaterloo.flix.TestAll")
    }
    maxParallelForks = 1
    maxHeapSize = "3g"
}

tasks.register<JavaExec>("testFuzzerSuite") {
    mainClass = "org.scalatest.tools.Runner"
    args = listOf("-s", "flix.fuzzers.FuzzerSuite", "-o")
    classpath = sourceSets["test"].runtimeClasspath
    standardInput = System.`in`
}

tasks.register<JavaExec>("testIDECompletion") {
    mainClass = "org.scalatest.tools.Runner"
    args = listOf("-s", "ca.uwaterloo.flix.api.lsp.TestCompletionProvider", "-o")
    classpath = sourceSets["test"].runtimeClasspath
    standardInput = System.`in`
}

tasks.register<Copy>("vscode") {
    dependsOn("classes", "jar")
    from("build/libs/flix.jar")
    into(providers.gradleProperty("dev.flix.vscode.project"))
}

tasks.register<JavaExec>("testPackageManager") {
    dependsOn("testClasses")
    mainClass = "org.scalatest.tools.Runner"
    args = listOf("-s", "ca.uwaterloo.flix.tools.pkg.PackageManagerSuite", "-o")
    classpath = sourceSets["test"].runtimeClasspath
    standardInput = System.`in`
}
