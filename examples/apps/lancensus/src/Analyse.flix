use Util.Stat
use Util.Stats
eff Analyse {
    def analyseByRepo(commits: Map[String, List[String]]): Stats
    def analyseByByte(repos: List[String]): Map[String, BigInt]
}

mod Analyse {
    use Json.JsonElement

    pub def runWithGithub(f: Unit -> a \ ef): a \ ef - Analyse + Github + AnalysisFailure + IO = {
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

    def commitToAddsDels(stat: Map[String, (BigInt, BigInt)], repo:String, sha: String): Map[String, (BigInt, BigInt)] \ AnalysisFailure + Github = {
        Github.getCommit(repo, sha) |> Util.accessJson("files") |> List.foldLeft((acc, file) -> {
            let filename =  Util.accessJson("filename", file);
            let extOpt =  filename |> Util.extractExt;
            match extOpt {
                case Option.None  => acc
                case Option.Some(ext) => 
                    let additions = Util.accessJson("additions", file);
                    let deletions = Util.accessJson("deletions", file);
                    match Map.get(ext, acc) {
                        case Option.None        => Map.insert(ext, (additions, deletions), acc)
                        case Option.Some((adds, dels)) => Map.insert(ext, (adds + additions, dels + deletions), acc)
                    }
            }
        }, stat)
    }
}
