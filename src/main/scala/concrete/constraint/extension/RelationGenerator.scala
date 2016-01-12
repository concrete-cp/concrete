package concrete.constraint.extension

trait RelationGenerator {
  def apply(data: Iterable[Seq[Int]]): Relation
}