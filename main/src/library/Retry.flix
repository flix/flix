// ---------- Stuff that shouldnt be here: ----------
// ----------------------- specific Handlers ---------------------

///
/// A handler for the Net.Dns effect that adds retries using the "engine". 
///
def dnsHandleWithRetry[ef1: Eff, ef2: Eff, b: Type](pol: Retry.Policy[ef1], f: Unit -> b \ ef2): b \ ef2 + ef1 + Time.Sleep + Sys.Console + Net.Dns =
    run {
        f()
    } with handler Net.Dns {
        def lookup(host, k) = k(Retry.retry(pol, Net.Dns.lookup)(host))
        def lookupAll(host, k) = k(Retry.retry(pol, Net.Dns.lookupAll)(host))
    }

// this uses the retry effect instead of just the "engine"
///
/// A handler for the Net.Dns effect that adds retries. 
///
def dnsHandleWithRetry2[ef2: Eff, b: Type](f: Unit -> b \ ef2): b \ (ef2 - Net.Dns) + Net.Dns + Time.Sleep + Retry = 
    run {
        f()
    } with handler Net.Dns {
        def lookup(host, k) = {
            def loop(attempts: Int32): b \ (ef2 - Net.Dns) + Net.Dns + Time.Sleep + Retry = {
                match Net.Dns.lookup(host) {
                    case Ok(ip) => k(Ok(ip))
                    case Err(e) => match Retry.ask(attempts) {
                        case None => k(Err(e))
                        case Some(dur) => {
                            Time.Sleep.sleep(dur);
                            loop(attempts + 1)
                        }
                    }
                }
            };
            loop(0)
        }
        def lookupAll(_host, _k) = unreachable!()
    }


// ---------------------------- actual code -----------------------------

pub eff Retry {
    def ask(attempts: Int32): Option[Time.Duration] 
}

mod Retry {
    use Policy.Policy
    use Time.Duration
    ///
    /// A retry policy. Consists of a function from the current retry count to an optional duration to wait.
    ///
    pub enum Policy[ef: Eff] {
        case Policy(Int32 -> Option[Duration] \ ef)
    }

    ///
    /// A policy that limits retries to a given number.
    ///
    pub def limitRetries(n: Int32): Policy[{}] \ {} =
        Policy(i -> {
            if (i < n) {Some(Time.Duration.zero())} else {None}}
        )

    def backoff(f: (Int32 -> Duration \ ef )): Policy[ef] =
        Policy(i -> {
            Some(f(i))
        })

    ///
    /// A policy that provides a constant backoff between retries. 
    ///
    pub def constantBackoff(dur: Duration): Policy[{}] = 
        backoff(_ -> dur)
    
    ///
    /// A policy that provides an exponential backoff between retries. 
    ///
    pub def expBackoff(): Policy[{}] = 
        backoff(i -> {
            Time.Duration.seconds(Int32.pow(base = i, 2))
        })
    
    ///
    /// A policy that makes it retry untill some condition is met.
    ///
    pub def until(f: Unit -> Bool \ ef): Policy[ef] \ {} =
        Policy(_i -> {
            if (not f()) Some(Duration.zero()) else None
        })

    ///
    /// Combines two policies, giving a policy that halts if either parameter halts, and otherwise waits the maximum duration.
    ///
    pub def combine(a: Policy[ef], b: Policy[ef2]): Policy[ef + ef2] = Policy(i -> {
            let (f1, f2) = match (a, b) {case (Policy(f1), Policy(f2)) => (f1, f2)};
            match (f1(i), f2(i)) {
                case (Some(r1), Some(r2)) => Some(Order.max(r1, r2))
                case _ => None
            }
        })

    ///
    /// A policy that halts after a given deadline.
    ///
    pub def deadline(startTime: Int64, deadline: Duration): Policy[{Time.Clock}] =
        Policy(_ -> {
            let elapsed = Time.Clock.now() - startTime;
            if (elapsed >= Time.Duration.toMillis(deadline)) {
                None
            } else {
                Some(Time.Duration.milliseconds(0))
            }
        })

    ///
    /// A method that adds jitter to a policy. Is best used as the final modification.
    ///
    pub def addJitter(maxJitter: Duration, pol: Policy[ef]): Policy[ef + Math.Random] =
        Policy(i -> {
            let (Policy(f)) = pol;
            match f(i) {
                case None => None
                case Some(dur) => {
                    let nanoJit = Time.Duration.toNanos(maxJitter);
                    let nanoJitF64 = Int64.toFloat64(nanoJit);
                    let nanoJitter: Option[Int64] = Float64.tryToInt64(nanoJitF64 * Math.Random.randomFloat64());
                    match nanoJitter {
                        case Some(jit) => Some(dur + Time.Duration.nanoseconds(jit))
                        case None => Some(dur)
                    }
                }
            }
        })
    ///
    /// The retry engine. Takes a policy and a function, and returns a function that behaves like the input function, but retries on errors according to the policy.
    ///
    pub def retry[ef1: Eff, a: Type, b: Type, e: Type, ef2: Eff]
    (pol: Policy[ef1], f: a -> Result[e, b] \ ef2): a -> Result[e, b] \ ef1 + ef2 + Time.Sleep + Sys.Console = 
        retryRec(pol, 0, f)
    
    def retryRec[ef1: Eff, a: Type, b: Type, e: Type, ef2: Eff]
    (pol: Policy[ef1], i: Int32, f: a -> Result[e, b] \ ef2): a -> Result[e, b] \ ef1 + ef2 + Time.Sleep + Sys.Console = 
        x -> {
            let res = f(x);
            match res {
                case Ok(ok) => Ok(ok)
                case Err(e) => { match pol {case Policy(g) => {
                    match g(i) {
                        case None => Err(e)
                        case Some(dur) => {
                            //todo: Remove print
                            Sys.Console.println("Waiting now " + ToString.toString(Time.Duration.toNanos(dur)) + " nanoseconds before retrying..."); 
                            Time.Sleep.sleep(dur);
                            retryRec(pol, i + 1, f, x) //tailrec?
                        }
                    }
                }}
                }
            }
        }
    ///
    /// Handler for the retry effect.
    ///
    pub def handle[ef1: Eff, b: Type, ef2: Eff](pol: Policy[ef1], f: Unit -> b \ ef2): b \ ef1 + (ef2 - Retry) =
        run {
            f()
        } with handler Retry {
            def ask(attempts, k) = {
                let Policy(g) = pol;
                match g(attempts) {
                    case None => k(None)
                    case Some(dur) => k(Some(dur))
                }
            }
        }
}