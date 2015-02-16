val neighs =
  Array(
    Array(1, 2, 3), // 0
    Array(0, 2, 4, 5), // 1
    Array(0, 1, 3, 4, 5, 6), // 2
    Array(0, 2, 5, 6), // 3
    Array(1, 2, 5, 7), // 4
    Array(1, 2, 3, 4, 6), // 5
    Array(2, 3, 5, 7), // 6
    Array(4, 5, 6) // 7
  )

def check(xs: Array[Int]): Boolean = {
  (0 until 8).forall { i =>
    val ns = neighs(i)
    ns.forall { n => math.abs(xs(i) - xs(n)) > 1 }
  }
}

(0 until 8).permutations.filter { xs =>
  check(xs.toArray)
}.map { xs =>
  xs.mkString(", ")
}.mkString("\n")