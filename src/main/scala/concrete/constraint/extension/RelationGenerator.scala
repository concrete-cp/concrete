package concrete.constraint.extension

trait RelationGenerator {
  def apply(data: Traversable[IndexedSeq[Int]]): Relation
}