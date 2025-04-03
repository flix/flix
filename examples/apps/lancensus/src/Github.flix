use Util.Stats
use Json.JsonElement

pub eff Github {
    def getRepos(): List[String]
    def getCommits(repos: List[String]): Map[String, List[String]]
    def getLanguages(repos: List[String]): Map[String, JsonElement]
    def getCommit(repo: String, sha: String): JsonElement
} 

mod Github {
    use Util.accessJson

    def buildHeader(): Map[String, List[String]] \ Config = {
        let token = Config.getToken();
        Map#{
            "Accept"                => "application/vnd.github+json" :: Nil,
            "Authorization"         => "Bearer ${token}" :: Nil,
            "X-Github-Api-Version"  => "2022-11-28" :: Nil
        }
    }
    
    pub def runWithConfigHttp(f: Unit -> a \ ef): a \ ef - Github + Config + HttpWithResult + AnalysisFailure = {
        run {
            f()
        } with handler Github {
            def getRepos(resume) = {
                Util.getAllPages("https://api.github.com/user/repos?affiliation=owner,collaborator&per_page=100", buildHeader())
                |> List.map(repoJson -> Util.accessJson("full_name", repoJson)) |> resume
            }

            def getCommits(repos, resume) = {
                List.foldLeft((acc, repo) -> {
                    let url = "https://api.github.com/repos/${repo}/commits?author=${Config.getName()}&per_page=100";
                    let shas = Util.getAllPages(url, buildHeader()) |> List.map(Util.accessJson("sha"));
                    Map.insert(repo, shas, acc)
                }, Map.empty(), repos) |> resume
            }
            
            def getLanguages(repos, resume) = {
                List.foldLeft((acc, repo) -> {
                    let url = "https://api.github.com/repos/${repo}/languages";
                    let res = Util.getAndParse(url, buildHeader());
                    Map.insert(repo, res, acc)
                }, Map.empty(), repos) |> resume
            }

           def getCommit(repo, sha, resume) = {
                let url = "https://api.github.com/repos/${repo}/commits/${sha}";
                Util.getAndParse(url, buildHeader()) |> resume
           } 

        }
    }
}
