use Sys.Env

/// Reads system properties: OS name, architecture, version, and
/// the number of available virtual processors.
def main(): Unit \ { Env, IO } =
    let name    = Env.getOsName();
    let arch    = Env.getOsArch();
    let version = Env.getOsVersion();
    let cpus    = Env.getVirtualProcessors();
    println("OS:   ${name}");
    println("Arch: ${arch}");
    println("Ver:  ${version}");
    println("CPUs: ${cpus}")
