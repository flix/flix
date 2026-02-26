mod Retry {
    use Retry.Policy
    use Policy.Policy
    type alias Duration = Int32


    pub enum Policy[ef: Eff] {
        case Policy(Int32 -> Option[Duration] \ ef)
    }

    pub def limitRetries(n: Int32): Policy[{}] \ {} =
        Policy(i -> {
            if (i < n) {Some(0)} else {None}}
        )
    
    def backoff(f: (Duration -> Duration \ ef )): Policy[ef] =
        Policy(i -> {
            Some(f(i))
        })

    pub def constantBackoff(time: Duration): Policy[{}] = 
        backoff(_i -> time)
    pub def expBackoff(): Policy[{}] = 
        backoff(i -> Int32.pow(base = 2, i))
    
    pub def until(f: Unit -> Bool \ ef): Policy[ef] \ {} =
        Policy(_i -> {
            if (not f()) Some(0) else None
        })

    pub def combine(a: Policy[ef], b: Policy[ef2]): Policy[ef + ef2] = Policy(i -> {
            let (f1, f2) = match (a, b) {case (Policy(f1), Policy(f2)) => (f1, f2)};
            match (f1(i), f2(i)) {
                case (Some(r1), Some(r2)) => Some(Int32.max(r1, r2))
                case _ => None
            }
        })

    pub def retry[ef1: Eff, a: Type, b: Type, e: Type, ef2: Eff]
    (pol: Policy[ef1], f: a -> Result[e, b] \ ef2): a -> Result[e, b] \ ef1 + ef2 + Clock + Console = 
        retryRec(pol, 0, f)
    
    def retryRec[ef1: Eff, a: Type, b: Type, e: Type, ef2: Eff]
    (pol: Policy[ef1], i: Int32, f: a -> Result[e, b] \ ef2): a -> Result[e, b] \ ef1 + ef2 + Clock + Console = 
        x -> {
            let res = f(x);
            match res {
                case Ok(ok) => Ok(ok)
                case Err(e) => { match pol {case Policy(g) => {
                    match g(i) {
                        case None => Err(e)
                        case Some(dur) => {
                            Console.println("Waiting now " + ToString.toString(dur) + " seconds.");
                            wait(dur);
                            retryRec(pol, i + 1, f, x) //tailrec?
                        }
                    }
                }}
                }
            }
        }
    
    //Helpers since sleep() doesnt exist
    def recDelay(f: Unit -> b \ ef, delay_t: Int64, now: Int64): b \ {Clock} + ef = 
        let newTime = Clock.currentTime(TimeUnit.Seconds);
        if (newTime > now + delay_t) {
            f()
        } else {
            recDelay(f, delay_t, now)
        }
    def wait(t: Int32): Unit \ Clock = 
        if (t == 0) {
            ()
        } else {
            let now = Clock.currentTime(TimeUnit.Seconds);
            let t_64 = Int32.toInt64(t);
            recDelay(() -> (), t_64, now)
        }
}