package concrete

sealed trait Event {
  def <=(e: Event): Boolean
}

object Event {
  def apply(before: Domain, after: Domain): Option[Event] = {
    if (before eq after) {
      None
    } else {
      assert(after.subsetOf(before))
      Some(InsideRemoval(before, after))
    }
  }

}

case object Assignment extends Event {
  def <=(e: Event) = true
}

/**
 * AT LEAST a bound was removed. Other values may have been removed as well
 */
case object BoundRemoval extends Event {
  def apply(after: Domain) = if (after.isAssigned) Assignment else BoundRemoval
  def <=(e: Event) = (e eq this) || InsideRemoval <= e
}

/**
 * Bounds are unchanged
 */
case object InsideRemoval extends Event {
  @inline
  def apply(before: Domain, after: Domain) =
    if (before.head != after.head || before.last != after.last) BoundRemoval(after)
    else InsideRemoval

  def <=(e: Event) = e eq this
}

