def newId(): Int64 & Impure = 
    import new flix.runtime.GlobalCounter() as newGlobalCounter;
    import flix.runtime.GlobalCounter:newId() as javaNewId;
    javaNewId(newGlobalCounter())

type alias Lock = ##java.util.concurrent.locks.Lock

type alias ReentrantLock = ##java.util.concurrent.locks.ReentrantLock

def newReentrantLock(): ReentrantLock & Impure = 
    import new java.util.concurrent.locks.ReentrantLock() as javaNewReentrantLock;
    javaNewReentrantLock()

type alias Condition = ##java.util.concurrent.locks.Condition

def newCondition(reentrantLock: ReentrantLock): Condition & Impure =
    import java.util.concurrent.locks.ReentrantLock.newCondition() as javaNewCondition;
    javaNewCondition(reentrantLock)

type alias LockConditionPair = (Lock, Condition)

type alias Channel[a] = {
    id: Int64,
    channelLock: ReentrantLock,
    bufferSize: Int32,
    unbuffered: Bool,
    elementQueue: MutList[a],
    waitingGetters: MutSet[LockConditionPair],
    waitingSetters: Condition
}

def newChannel(bufferSize: Int32): Channel & Impure =
    let unbuffered = default;
    if (bufferSize < 0) 
        exit(1)
    else if (bufferSize == 0) {
        ref bufferSize := 1;
        unbuffered := true
    }
    else 
        unbuffered := false;

    let mutList = MutList.new();
    MutList.reserve!(bufferSize, mutList);

    let reentrantLock = newReentrantLock();

    {
        id = newId(),
        channelLock = reentrantLock,
        bufferSize = bufferSize,
        unbuffered = unbuffered,
        elementQueue = mutList,
        waitingGetters = MutSet.empty(),
        waitingSetters = newCondition(reentrantLock)
    }

def exit(status: Int32): () & Impure = 
    import java.lang.Runtime:getRuntime();
    let runtime = getRuntime();
    import java.lang.Runtime.exit(Int32);
    runtime.exit(status)

def todo(message: Option[String]): () & Impure = 
    match message {
        case Some(str) => Console.printLine(str),
        case None => Console.printLine("TODO")
    };
    exit(1)

def main(): () & Impure =
    Console.print("Hello, World!")