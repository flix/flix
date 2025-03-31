eff Config {
    def getName(): String 
    def getToken(): String
}

mod Config {
    def getEnv(envName: String): String \ Environment + AnalysisFailure = {
        let envVarOpt = Environment.getEnv() |> Map.get(envName);
        match envVarOpt {
            case Some(envVar) => envVar
            case None => AnalysisFailure.raiseTokenError("Environment variable ${envName} not found.")
        }
    }

    pub def runWithEnvironment(f: Unit -> a \ ef): a \ ef - Config + AnalysisFailure + Environment = {
        run {
            f()
        } with handler Config {
            def getName(resume) = getEnv("GITHUB_NAME") |> resume
            def getToken(resume) = getEnv("GITHUB_TOKEN") |> resume
        }
    }
}
