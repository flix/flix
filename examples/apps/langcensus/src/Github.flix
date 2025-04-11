use Util.Stats
use Json.JsonElement

///
/// Gets all sorts of data from github.
/// Will get the user and token through Config effect.
///
pub eff Github {
    ///
    /// Gets a list of the full name of reporitories that the user has contributed to, regardless of privacy.
    ///
    def getRepos(): List[String]

    ///
    /// Gets all the commits from the user in the given repositories.
    /// 
    /// Returns a map from the repository name to a list of commit shas.
    ///
    def getCommits(repos: List[String]): Map[String, List[String]]

    ///
    /// Gets all the language data from the given repository names.
    /// 
    /// Returns a map from the repository name to the json of lanuage data.
    /// The language data should be a map from laguange name to the byte of code for this languge in this repository.
    ///
    def getLanguages(repos: List[String]): Map[String, JsonElement]
    
    ///
    /// Gets the detailed statistic of a specific commit in the repository.
    ///
    def getCommit(repo: String, sha: String): JsonElement
} 

mod Github {
    use Util.accessJson

    ///
    /// The general header for a github request.
    ///
    def buildHeader(): Map[String, List[String]] \ Config = {
        let token = Config.getToken();
        Map#{
            "Accept"                => "application/vnd.github+json" :: Nil,
            "Authorization"         => "Bearer ${token}" :: Nil,
            "X-Github-Api-Version"  => "2022-11-28" :: Nil
        }
    }
    
    ///
    /// The handler that gets data from github through REST API.
    ///
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
