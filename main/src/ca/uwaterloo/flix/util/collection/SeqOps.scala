package ca.uwaterloo.flix.util.collection

object SeqOps {

  /**
    * Gets duplicate pairs from a list of items.
    * This is used to generate a list of pairs that can be mapped into Duplicate errors.
    * What constitutes a "duplicate" is abstracted into the groupBy argument.
    * But for enum variants, two variants are duplicates if they share names.
    */
  def getDuplicates[A, K](items: Seq[A], groupBy: A => K): List[(A, A)] = {
    val groups = items.groupBy(groupBy)
    for {
      (_, group) <- groups.toList
      // if a group has a nonempty tail, then everything in the tail is a duplicate of the head
      duplicate <- group.tail
    } yield (group.head, duplicate)
  }

}
