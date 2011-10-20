package cspfj.constraint;

trait ResidueManager {

    def getResidue(position: Int, index: Int): Array[Int]

    def updateResidue(residue: Array[Int])

    def remove(tuple: Array[Int])

}