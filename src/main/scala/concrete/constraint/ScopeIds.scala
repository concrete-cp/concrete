package concrete.constraint

trait ScopeIds extends Constraint {
  val ids: Array[Int] = scope.map(_.id)
}