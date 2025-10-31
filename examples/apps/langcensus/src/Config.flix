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
    def getEnv(envName: String): String \ Env + AnalysisFailure = {
        let envVarOpt = Env.getEnv() |> Map.get(envName);
        match envVarOpt {
            case Some(envVar) => envVar
            case None => AnalysisFailure.raiseConfigError("Environment variable ${envName} not found.")
        }
    }

    ///
    /// Handles Config effect through accessing environment variables.
    ///
    pub def runWithEnv(f: Unit -> a \ ef): a \ ef - Config + AnalysisFailure + Env = {
        run {
            f()
        } with handler Config {
            def getName(resume) = getEnv("GITHUB_NAME") |> resume
            def getToken(resume) = getEnv("GITHUB_TOKEN") |> resume
        }
    }
}
