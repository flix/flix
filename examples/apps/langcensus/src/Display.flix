use Util.Stats

///
/// The effect to display different analysis results.
///
eff Display {
    def displayByUser(stat: Stats): Unit
    def displayByRepo(stat: Stats): Unit
    def displayByByte(stat: Map[String, BigInt]): Unit
}

mod Display{
    use Util.Stat

    ///
    /// Handles the display effect through IO.
    ///
    pub def runWithIO(f: Unit -> a \ ef): a \ ef - Display + IO = {
        run {
            f()
        } with handler Display {
            def displayByRepo(stat, cont) =  {
                println("\n==================== Analysis Result Per Repo ====================\n");
                Map.forEach((repo, thisStat) -> {
                    println("\nRepository: ${repo}");
                    thisStat |> statFilter |> renameAndMerge |> sortMapByValue |> printEachLanguage              
                }, stat) |> cont
            }
            
            def displayByUser(stats, resume) = {
                println("\n==================== Analysis Result For User ====================\n");
                Map.foldLeft((acc, stat) -> {
                    Map.foldLeftWithKey((acc1, ext, data) -> {
                        Map.insertWith(match (oldAdds, oldDels) -> match (newAdds, newDels) -> 
                            (oldAdds + newAdds, oldDels + newDels)
                        , ext, data, acc1)
                    }, acc, stat)
                }, Map.empty(), stats) |> statFilter |> renameAndMerge |> sortMapByValue |> printEachLanguage |> resume
            }
            
            def displayByByte(stat, resume) = {
                println("\n==================== Analysis Result By Bytes ====================\n");
                stat 
                |> sortMapByValue 
                |> List.forEach(match (lang, size) -> println("${lang}: ${size} Bytes")) 
                |> resume
            }
        }
    }
    
    ///
    /// Filters out the languages whose adds is zero.
    ///
    def statFilter(stat: Stat): Stat = Map.filter(match (adds, _) -> adds > 0ii, stat)
        
    ///
    /// Prints the stat for each language.
    ///
    def printEachLanguage(stat: List[(String, (BigInt, BigInt))]): Unit \ IO = 
        List.forEach(match (language, (adds, dels)) -> println("${language}: ${adds} additions, ${dels} deletions."), stat)
        
    ///
    /// Renames the extention to langauges and merge entries of the same language.
    /// 
    def renameAndMerge(stat: Stat): Stat = 
        Map.foldLeftWithKey((acc, ext, data) -> {
            match ext2Language(ext) {
                case Option.None        => acc
                case Option.Some(language) => 
                    Map.insertWith(match (oldAdds, oldDels) -> match (newAdds, newDels) -> 
                        (oldAdds + newAdds, oldDels + newDels)
                    , language, data, acc)
            }
        }, Map.empty(), stat)

    ///
    /// Sorts the map by converting it to a list.
    ///
    def sortMapByValue(map: Map[k, v]): List[(k, v)] with Order[v] = Map.toList(map) |> List.sortBy(match (_, v) -> v)

    ///
    /// A map from extension to list.
    ///
    def ext2Language(ext: String): Option[String] = 
        let mapping: Map[String, String] = Map#{
            "ASM" => "Assembly",
            "asm" => "Assembly",
            "bat" => "Batch Script",
            "c" => "C",
            "cls" => "TeX Class",
            "cpp" => "C++",
            "cs" => "C#",
            "css" => "CSS",
            "dart" => "Dart",
            "ex" => "Elixir",
            "flix" => "Flix",
            "fs" => "F#",
            "go" => "Go",
            "gradle" => "Gradle",
            "h" => "C Header",
            "hs" => "Haskell",
            "html" => "HTML",
            "ipynb" => "Jupyter Notebook",
            "java" => "Java",
            "js" => "JavaScript",
            "jsx" => "JavaScript JSX",
            "kt" => "Kotlin",
            "lds" => "Linker Script",
            "lua" => "Lua",
            "m" => "Matlab",
            "md" => "Markdown",
            "ml" => "OCaml",
            "mli" => "OCaml Interface",
            "mly" => "Menhir",
            "opam" => "OPAM",
            "php" => "PHP",
            "pl" => "Perl",
            "ps1" => "PowerShell",
            "py" => "Python",
            "r" => "R",
            "rb" => "Ruby",
            "rs" => "Rust",
            "s" => "Assembly",
            "S" => "Assembly",
            "scala" => "Scala",
            "scss" => "Sass",
            "sh" => "Shell Script",
            "sql" => "SQL",
            "swift" => "Swift",
            "sy" => "SystemVerilog",
            "tex" => "TeX",
            "toml" => "TOML",
            "ts" => "TypeScript",
            "tsx" => "TypeScript JSX",
            "typ" => "Typst",
            "vim" => "Vim script",
            "vue" => "Vue",
            "yaml" => "YAML",
            "yml" => "YAML"
        };
        Map.get(ext, mapping)
}
