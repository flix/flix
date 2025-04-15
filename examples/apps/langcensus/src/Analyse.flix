use Util.Stat
use Util.Stats

///
/// The effect to analyse a user's github data.
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
    use Util.Stat
    use Json.JsonElement

    ///
    /// Handles analysis effect through Github Effect.
    ///
    pub def runWithGithub(f: Unit -> a \ ef): a \ ef - Analyse + Github + AnalysisFailure = {
        run {
            f()
        } with handler Analyse {
            def analyseByRepo(commits, resume) = 
            // Map.foldLeftWithKey((acc, repo, shas) -> {
            //     let stat = List.foldLeft((acc1, sha) -> {
            //         commitToAddsDels(acc1, repo, sha)
            //     }, Map.empty(), shas);
            //     Map.insert(repo, stat, acc)
            // }, Map.empty(), commits) |> resume
                let allStats : List[(String, Stat)] = foreach (
                    (repo, shas) <- commits;
                    sha <- shas
                ) yield commitToAddsDels(repo, sha);
                mergeStats(allStats) |> resume

            def analyseByByte(repos, resume) = {
                List.foldLeft((acc, repo) -> {
                    let languageStat = Github.getLanguages(repo);
                    let languageStatMap = Util.fromJson(languageStat);
                    Map.foldLeftWithKey((acc1, lang, size) -> {
                        let sz = Util.fromJson(size);
                        match Map.get(lang, acc1) {
                            case None => Map.insert(lang, sz, acc1)
                            case Some(v) => Map.insert(lang, sz + v, acc1)
                        }
                    }, acc, languageStatMap)
                }, Map.empty(), repos) |> resume
                // let allStat: List[(String, BigInt)] = foreach (
                //     repo <- repos;
                //     languageStat = Github.getLanguages(repo);
                //     languageStatMap = Util.fromJson(languageStat);
                //     (lang, size) <- languageStatMap;
                //     sz = Util.fromJson(size)
                // ) yield (lang, sz);
                // mergeStatByByte(allStat) |> resume
            }
        }
    }

    def commitToAddsDels(repo:String, sha: String): (String, Stat) \ AnalysisFailure + Github = {
        let stat = Github.getCommit(repo, sha) |> Util.accessJson("files") |> List.foldLeft((acc, file) -> {
            let filename =  Util.accessJson("filename", file);
            let extOpt =  filename |> Util.extractExt;
            match extOpt {
                case Option.None  => acc
                case Option.Some(ext) => 
                        let additions = Util.accessJson("additions", file);
                        let deletions = Util.accessJson("deletions", file);
                        match Map.get(ext, acc) {
                        case Option.None                   => Map.insert(ext, (additions, deletions), acc)
                            case Option.Some((adds, dels)) => Map.insert(ext, (adds + additions, dels + deletions), acc)
                        }
                }
        }, Map.empty());
        (repo, stat)
    }
    
    def mergeStats(stats: List[(String, Stat)]): Map[String, Stat] = 
        List.foldLeft((acc, data) -> {
            let (repo, stat) = data;
            match Map.get(repo, acc) {
                case None => Map.insert(repo, stat, acc)
                case Some(oldStat) => 
                    let newStat = mergeStat(oldStat, stat);
                    Map.insert(repo, newStat, acc)
            }
        } , Map.empty(), stats)
        
    def mergeStat(oldStat: Stat, stat: Stat): Stat = {
        Map.foldLeftWithKey((acc, lang, data) -> {
            let (adds, dels) = data;
            match Map.get(lang, acc) {
                case Option.None        => Map.insert(lang, data, acc)
                case Option.Some(oldData) => 
                    let (oldAdds, oldDels) = oldData;
                    Map.insert(lang, (adds + oldAdds, dels + oldDels), acc)
            } 
        }, oldStat, stat)
    }
}
