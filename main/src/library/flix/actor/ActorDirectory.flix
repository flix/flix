///
/// An actor directory, used to retrieve mailboxes of actors in the system
///
type alias ActorDirectory = RunningActorSystem

// TODO: do we need to decouple that from ActorSystem? They could just be merged.
namespace ActorDirectory {
    ///
    /// Returns an actor mailbox (blocking function)
    ///
    pub def getActor(s: ActorDirectory, name: Str): Option[Mailbox] = {
      let RunningActorSystem(sys) = s;
      let c = chan Option[Mailbox] 0;
      sys <- GetActor(name, c);
      <- c
    }

    ///
    /// Sends a message to an actor, given its name
    ///
    pub def send(s: RunningActorSystem, actorName: Str, msg: Message): Bool = {
        let RunningActorSystem(sys) = s;
        let c = chan Bool 0;
        sys <- SendMessage(actorName, msg, c);
        <- c
    }

    ///
    /// Dynamically create a new actor, given its parent name, its behavior, and category
    ///
    pub def create(s: RunningActorSystem, parent: Str, behavior: ActorBehavior, category: Str): Mailbox = {
        let RunningActorSystem(sys) = s;
        let c = chan Mailbox 0;
        sys <- CreateActor(parent, behavior, category, c);
        <- c
    }
}

