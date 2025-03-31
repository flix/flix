use Util.Stats

eff Display {
    def displayByUser(stat: Stats): Unit
    def displayByRepo(stat: Stats): Unit
    def displayByByte(stat: Map[String, BigInt]): Unit
}

mod Display{
    use Util.Stat

    pub def runWithIO(f: Unit -> a \ ef): a \ ef - Display + IO = {
        run {
            f()
        } with handler Display {
            def displayByRepo(stat, cont) =  {
                println("\n==================== Analysis Result Per Repo ====================\n");
                Map.forEach((repo, thisStat) -> {
                    println("\nRepository: ${repo}");
                    thisStat |> statFilter |> renameAndMerge |> sortMapByList |> printEachLanguage              
                }, stat) |> cont
            }
            
            def displayByUser(stats, resume) = {
                println("\n==================== Analysis Result For User ====================\n");
                Map.foldLeft((acc, stat) -> {
                    Map.foldLeftWithKey((acc1, ext, data) -> {
                        let (adds, dels) = data;
                        match Map.get(ext, acc) {
                            case Option.None        => Map.insert(ext, (adds, dels), acc1)
                            case Option.Some(newData) => let (newAdds, newDels) = newData; Map.insert(ext, (adds + newAdds, dels + newDels), acc1)
                        }
                    }, acc, stat)
                }, Map.empty(), stats) |> statFilter |> renameAndMerge |> sortMapByList |> printEachLanguage |> resume
            }
            
            def displayByByte(stat, resume) = {
                println("\n==================== Analysis Result By Bytes ====================\n");
                stat |> sortMapByList |>  List.forEach(data -> {
                    let (lang, size) = data;
                    println("${lang}: ${size} Bytes")
                }) |> resume
            }
        }
    }
    
    def statFilter(stat: Stat): Stat = {
        Map.filter(lines -> {
            let (adds, _) = lines;
            adds > 0ii
        }, stat)
    }
        
    def printEachLanguage(stat: List[(String, (BigInt, BigInt))]): Unit \ IO = 
        List.forEach(item -> {
            let (language, (adds, dels)) = item;
            println("${language}: ${adds} additions, ${dels} deletions.")
        }, stat)
        
    def renameAndMerge(stat: Stat): Stat = 
        Map.foldLeftWithKey((acc, ext, data) -> {
            let (adds, dels) = data;
            match ext2Language(ext) {
                case Option.None        => acc
                case Option.Some(language) => 
                    match Map.get(language, acc) {
                        case Option.None        => Map.insert(language, (adds, dels), acc)
                        case Option.Some(newData) => 
                            let (newAdds, newDels) = newData;
                            Map.insert(language, (adds + newAdds, dels + newDels), acc)
                    }
            }
        }, Map.empty(), stat)

    def sortMapByList(map: Map[k, v]): List[(k, v)] with Order[v] = {
        let list = Map.toList(map);
        List.sortBy(kv -> {
            let (_, v) = kv;
            v
        }, list)
    }

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
