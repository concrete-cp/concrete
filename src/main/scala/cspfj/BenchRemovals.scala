package cspfj

object BenchRemovals {

  def main(args: Array[String]) {
    val a = (0 to 1000).toIndexedSeq
    val b = (0 to 1000).toIndexedSeq

    println(bench {
      (a, b).zipped.filter((a, b) => a > 50)._2
    })

    println(bench {
      (a, b).zip.iterator.filter(t => t._1 > 50).map(t => t._2)

    })

    println(bench {
      a.zip(b).filter(t => t._1 > 50).map(t => t._2)
    })

    println(bench {
      a.iterator.zip(b.iterator).filter(t => t._1 > 50).map(t => t._2).toList
    })
  }

  def bench(f: => Unit): Long = {
    var count = 100000
    val start = -System.currentTimeMillis

    while (count > 0) {
      f
      count -= 1
    }

    start + System.currentTimeMillis
  }

}