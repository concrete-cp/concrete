package concrete

import org.scalatest.FlatSpec

class EventTest extends FlatSpec {
  "events" should "be correctly ordered" in {
    assert(Assignment <= BoundRemoval)
    assert(BoundRemoval <= InsideRemoval)
    assert(Assignment <= InsideRemoval)
    assert(!(InsideRemoval <= BoundRemoval))
    
  }
}