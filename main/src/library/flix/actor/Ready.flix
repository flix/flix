///
/// A special message sent by an actor to indicate that it is ready to receive messages.
///
pub enum Ready {
    case Ready // TODO: Extend with another message: Unavailable which means that the actor cannot start? If so, rename the enum
}
