def behindMaster(g: #{Commit(Hash, String, Hash)}, branch: String): Bool =
    let p = #{
        Branch(hash) :- Commit(hash, branch, parent),
                        Commit(parent, "master", _).
        NewCommit() :- Commit(_, "master", hash), Branch(hash).
    };
    (solve g <+> p) |= NewCommit().
