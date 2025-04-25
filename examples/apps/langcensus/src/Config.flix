///
/// The effect to get the user configuration.
///
eff Config {
    ///
    /// Gets the github user name.
    ///
    def getName(): String

    ///
    /// Gets the github user token.
    ///
    def getToken(): String
}

mod Config {
    ///
    /// Gets an environment variable.
    ///
    def getEnv(envName: String): String \ Environment + AnalysisFailure = {
        let envVarOpt = Environment.getEnv() |> Map.get(envName);
        match envVarOpt {
            case Some(envVar) => envVar
            case None => AnalysisFailure.raiseConfigError("Environment variable ${envName} not found.")
        }
    }

    ///
    /// Handles Config effect through accessing environment variables.
    ///
    pub def runWithEnvironment(f: Unit -> a \ ef): a \ ef - Config + AnalysisFailure + Environment = {
        run {
            f()
        } with handler Config {
            def getName(resume) = getEnv("GITHUB_NAME") |> resume
            def getToken(resume) = getEnv("GITHUB_TOKEN") |> resume
        }
    }
}
