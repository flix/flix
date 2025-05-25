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
                let allStats : List[(String, Stat)] = foreach (
                    (repo, shas) <- commits;
                    sha <- shas
                ) yield commitToAddsDels(repo, sha);
                mergeStats(allStats) |> resume

            def analyseByByte(repos, resume) = {
                let allStats: List[(String, BigInt)] = foreach (
                    repo <- repos;
                    languageStat = Github.getLanguages(repo);
                    languageStatMap = Util.fromJson(languageStat);
                    (lang, size) <- (languageStatMap: Map[String, BigInt])
                ) yield (lang, size);
                mergeStatsByByte(allStats) |> resume
            }
        }
    }

    ///
    /// Collect adds and dels from all changed valid files in a commit.
    /// 
    def commitToAddsDels(repo: String, sha: String): (String, Stat) \ AnalysisFailure + Github = {
        let stat = Github.getCommit(repo, sha) |> Util.accessJson("files") |> List.foldLeft((acc, file) -> {
            let filename =  Util.accessJson("filename", file);
            let extOpt =  filename |> Util.extractExt;
            match extOpt {
                case Option.None  => acc
                case Option.Some(ext) => 
                    let additions = Util.accessJson("additions", file);
                    let deletions = Util.accessJson("deletions", file);
                    Map.insertWith(
                        match (old_adds, old_dels) -> match (new_adds, new_dels) -> (old_adds + new_adds, old_dels + new_dels),
                        ext, (additions, deletions), acc
                    )
                }
        }, Map.empty());
        (repo, stat)
    }
    
    ///
    /// Convert a list of repository name and stat to a map from language name to stat, with stats for the same repository merged.
    ///
    def mergeStats(stats: List[(String, Stat)]): Map[String, Stat] = 
        List.foldLeft(
            match (acc) -> match (repo, stat) -> Map.insertWith(mergeStat, repo, stat, acc),
            Map.empty(), stats
        )
        
    ///
    /// Merge the stat from one commit to the accumulated stat, which should belong to the same repository.
    ///
    def mergeStat(oldStat: Stat, stat: Stat): Stat = {
        Map.foldLeftWithKey((acc, lang, data) -> {
            Map.insertWith(
                match (oldAdds, oldDels) -> match (newAdds, newDels) -> (oldAdds + newAdds, oldDels + newDels),
                lang, data, acc
            )
        }, oldStat, stat)
    }
    
    ///
    /// Convert a list of langauge name and bytes to a map from language name to bytes, with bytes for the same language merged.
    ///
    def mergeStatsByByte(allStat: List[(String, BigInt)]): Map[String, BigInt] = 
        List.foldLeft(
            match (acc) -> match(lang, bytes) -> Map.insertWith((old_bytes, new_bytes) -> old_bytes + new_bytes, lang, bytes, acc), 
            Map.empty(), allStat
        )
}
