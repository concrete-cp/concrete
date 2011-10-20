package cspfj.constraint.extension;

trait Matrix  {

    def set(tuple: Array[Int], status: Boolean)

    def check(tuple: Array[Int]): Boolean

    def copy: Matrix

    def isEmpty: Boolean

}
