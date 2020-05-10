package concrete

import org.scalatest.flatspec.AnyFlatSpec

class EventTest extends AnyFlatSpec {
  "events" should "be correctly ordered" in {
    assert(Assignment <= BoundRemoval)
    assert(BoundRemoval <= InsideRemoval)
    assert(Assignment <= InsideRemoval)
    assert(!(InsideRemoval <= BoundRemoval))
    
  }
}