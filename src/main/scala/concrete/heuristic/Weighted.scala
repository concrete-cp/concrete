package concrete.heuristic

trait Weighted {
  var _weight = 1
  def weight = _weight
  def weight_=(w: Int) { _weight = w }
}