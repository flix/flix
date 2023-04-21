pub def orphansAndZombies(processes: List[(processId, String, processId)], rootId: processId): {orphans = List[processId], zombies = List[processId]} with Order[processId] =
    let p = inject processes into Process;
    let lp = #{
        Zombie(pid) :- Process(pid, "dead", parent), Process(parent, "alive", _).
        HasParent(pid) :- Process(pid, _, parent), Process(parent, _, _).
        Orphan(pid) :- Process(pid, _, rootId).
        Orphan(pid) :- Process(pid, _, _), not HasParent(pid).
    };
    let solution = solve p, lp;
    let zombies = query solution select pid from Zombie(pid);
    let orphans = query solution select pid from Orphan(pid);
    {zombies = zombies, orphans = orphans}
