def allTests(): String = "hey"

def simple2Statement(): Bool = 1;; 2

def simple3Statement(): Int = 1;; 2;; 3

def simpleMultitypeStatement(): Int = "test";; 1

def combinationWithLet(): Int = let x = 2; 2;; x

def combinationWithLet(): String = let x = "hey"; {let x = 2; 2};; x