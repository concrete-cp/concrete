package concrete
package heuristic
package revision

final class Weight extends Key[Weighted] {
  def getKey(c: Weighted, s: ProblemState) = c.weight
}