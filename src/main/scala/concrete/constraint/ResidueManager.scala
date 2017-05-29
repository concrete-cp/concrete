package concrete.constraint


trait ResidueManager {
  def getResidue(position: Int, index: Int): Array[Int]

  def updateResidue(tuple: Array[Int]): Unit

  def remove(tuple: Array[Int]): Unit
}