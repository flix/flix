use Util.Stat
use Util.Stats

///
/// The effect to analyse the a user's github data.
///
eff Analyse {
    ///
    /// Analyses the data in the granularity of repositories.
    ///
    def analyseByRepo(commits: Map[String, List[String]]): Stats

    ///
    /// Analyses the data by counting the byte of code for each language.
    ///
    def analyseByByte(repos: List[String]): Map[String, BigInt]
}

mod Analyse {
    use Json.JsonElement

    ///
    /// Handles analysis effect through Github Effect.
    ///
    pub def runWithGithub(f: Unit -> a \ ef): a \ ef - Analyse + Github + AnalysisFailure = {
        run {
            f()
        } with handler Analyse {
            def analyseByRepo(commits, resume) = Map.foldLeftWithKey((acc, repo, shas) -> {
                let stat = List.foldLeft((acc1, sha) -> {
                    commitToAddsDels(acc1, repo, sha)
                }, Map.empty(), shas);
                Map.insert(repo, stat, acc)
            }, Map.empty(), commits) |> resume
            def analyseByByte(repos, resume) = {
                let languageStats = Github.getLanguages(repos);
                Map.foldLeft((acc, languageStat) -> {
                    let languageStatMap = Util.fromJson(languageStat);
                    Map.foldLeftWithKey((acc1, lang, size) -> {
                        let sz = Util.fromJson(size);
                        match Map.get(lang, acc1) {
                            case None => Map.insert(lang, sz, acc1)
                            case Some(v) => Map.insert(lang, sz + v, acc1)
                        }
                    }, acc, languageStatMap)
                }, Map.empty(), languageStats) |> resume
            }
        }
    }

    ///
    /// Gets the adds and dels for each language for a specific commit.
    ///
    def commitToAddsDels(stat: Map[String, (BigInt, BigInt)], repo:String, sha: String): Map[String, (BigInt, BigInt)] \ AnalysisFailure + Github = {
        Github.getCommit(repo, sha) |> Util.accessJson("files") |> List.foldLeft((acc, file) -> {
            let filename =  Util.accessJson("filename", file);
            if (Util.isGeneratedFilePath(filename)) 
                acc
            else match Util.extractExt(filename) {
                    case Some (ext) => 
                        let additions = Util.accessJson("additions", file);
                        let deletions = Util.accessJson("deletions", file);
                        match Map.get(ext, acc) {
                            case Option.None               => Map.insert(ext, (additions, deletions), acc)
                            case Option.Some((adds, dels)) => Map.insert(ext, (adds + additions, dels + deletions), acc)
                        }
                    case None => acc
                }
        }, stat)
    }
}
