///
/// Gets the language usage statistics for the user.
///
/// Will print three kinds of statictics:
///   - The adds and dels of the user for each language for each repository.
///   - The total adds and dels of the user for each language.
///   - The total bytes of code for each language for all the reporitorites the user has paticipated in.
///
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
