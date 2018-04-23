@test
def putChannel01(): Int = let ch = channel Int; let _ = ch <- 2; <- ch