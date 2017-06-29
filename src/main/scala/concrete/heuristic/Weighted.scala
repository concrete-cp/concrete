package concrete.heuristic

trait Weighted {
  var weight = 1

  def incrementWeight(): Unit = {
    //println(s"$this ${weight.Internal.downStream}")
    weight += 1
  }
}