def orphansAndZombies(g: #{Process(Pid, String, Pid)}): (#{Orphan(Pid)}, #{Zombie(Pid)}) =
    let p = #{
        Zombie(pid) :- Process(pid, "dead", parent), Process(parent, "alive", _).
        HasParent(pid) :- Process(pid, _, parent), Process(parent, _, _).
        Orphan(pid) :- Process(pid, _, 1).
        Orphan(pid) :- Process(pid, _, _), not HasParent(pid).
    };
    let m = solve (g <+> p);
    (project Orphan m, project Zombie m)
