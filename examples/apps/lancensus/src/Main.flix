def main(): Unit \ IO + Sys + Net =
    run {
        let repos = Github.getRepos();
        let repoShaMap = Github.getCommits(repos);
        let statByRepo = Analyse.analyseByRepo(repoShaMap);
        Display.displayByRepo(statByRepo);
        Display.displayByUser(statByRepo);
        let statByByte = Analyse.analyseByByte(repos);
        Display.displayByByte(statByByte)
    } with Display.runWithIO
    with Analyse.runWithGithub
    with Github.runWithConfigHttp
    with Config.runWithEnvironment
    with HttpWithResult.runWithIO
    with Environment.runWithIO
    with AnalysisFailure.handleAnalysisFailure
    with Exit.runWithIO
