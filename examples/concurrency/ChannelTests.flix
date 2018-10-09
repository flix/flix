def allTests(): Str = "hey"

def simple2Statement(): Int = 1;; 2

def simple3Statement(): Int = 1;; 2;; 3

def simpleMultitypeStatement(): Int = "test";; 1

def combinationWithLetInt(): Int = let x = 2; 2;; x

def combinationWithLetString(): Str = let x = "hey"; {let x = 2; 2};; x