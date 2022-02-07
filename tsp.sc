// input: two-dimensional array with distances between cities
// output: length of shortest route
def tsp_simple(distances: Array[Array[Double]]):Double = {
  // number of cities
  val n = distances.length
  // size of set S (combinations of all cities)
  val S_size = 1 << n

  val l = Array.ofDim[Double](S_size, n)

  for (s <- l.indices) // iterate over set S (combinations of cities)
    for (i <- l(s).indices) // iterate over cities
      if (s == 0)
        l(s)(i) = distances(i)(n - 1)
      else {
        // l(s)(i) = min{distances(i)(j) + l(S - {j})(j) | j aus S}
        // s & 2^j
        l(s)(i) = (0 until n)
          .filter(j => ((s >> j) & 1) == 1)
          .map(j => distances(i)(j) + l(s & ~(1 << j))(j))
          .min
      }

  l(S_size - 1)(n - 1)
}

// input: two-dimensional array with distances between cities
// output: length of shortest route and sequence of cities for this route (first to last)
def tsp(distances: Array[Array[Double]]):(Double, List[Int]) = {
  // number of cities
  val n = distances.length
  // size of set S (combinations of all cities)
  val S_size = 1 << n

  // contains tuple with l(i,s) and path to this element as list of previous cities
  val l = Array.ofDim[(Double, List[Int])](S_size, n)

  for (s <- l.indices) // iterate over combinations of cities (set S)
    for (i <- l(s).indices) // iterate over cities
      if (s == 0)
        l(s)(i) = (distances(i)(n - 1), Nil)
      else
        // l(s)(i) = min{distances(i)(j) + l(S - {j})(j) | j aus S}
        l(s)(i) = (0 until n)
          .filter(j => ((s >> j) & 1) == 1)
          .map(j => (distances(i)(j) + l(s & ~(1 << j))(j)._1, j :: l(s & ~(1 << j))(j)._2))
          .minBy(_._1)

  l(S_size - 1)(n - 1)
}

val Inf = Double.PositiveInfinity
val distances0 = Array(
  Array(0.0, 3.0, Inf, Inf, Inf),
  Array(Inf, 0.0, 2.0, Inf, Inf),
  Array(Inf, Inf, 0.0, 2.0, Inf),
  Array(Inf, Inf, Inf, 0.0, 4.0),
  Array(Inf, Inf, Inf, Inf, 0.0)
)
println(tsp_simple(distances0))
println(tsp(distances0))

val distances1 = Array(
  Array(0.0, 3.0, Inf, Inf, Inf),
  Array(Inf, 0.0, 2.0, Inf, Inf),
  Array(Inf, Inf, 0.0, 2.0, Inf),
  Array(Inf, Inf, Inf, 0.0, 4.0),
  Array(1.0, Inf, Inf, Inf, 0.0)
)
println(tsp_simple(distances0))
println(tsp(distances0))

val distances2 = Array(
  Array(0.0, Inf, 2.0, Inf, 4.0),
  Array(Inf, 0.0, Inf, 3.0, 1.0),
  Array(2.0, Inf, 0.0, 2.0, Inf),
  Array(Inf, 3.0, 2.0, 0.0, Inf),
  Array(3.0, 1.0, Inf, Inf, 0.0)
)
println(tsp_simple(distances2))
println(tsp(distances2))

val distances3 = Array(
  Array(0.0, Inf, 2.0, Inf, 3.0),
  Array(Inf, 0.0, Inf, 3.0, 1.0),
  Array(2.0, Inf, 0.0, 2.0, Inf),
  Array(Inf, 3.0, 2.0, 0.0, Inf),
  Array(4.0, 1.0, Inf, Inf, 0.0)
)
println(tsp_simple(distances3))
println(tsp(distances3))

val distances4 = Array(
  Array(0.0, 39.0, 22.0, 59.0, 54.0, 33.0, 57.0, 21.0, 89.0, 73.0),
  Array(39.0, 0, 20.0, 20.0, 81.0, 8.0, 49.0, 64.0, 63.0, 84.0),
  Array(22.0, 20.0, 0.0, 39.0, 74.0, 18.0, 60.0, 44.0, 71.0, 73.0),
  Array(59.0, 20.0,.0, 39.0, 0.0, 93.0, 27.0, 51.0, 81.0, 48.0, 80.0),
  Array(54.0, 81.0, 74.0, 93.0, 0.0, 73.0, 43.0, 56.0, 104.0, 76.0),
  Array(33.0, 8.0, 18.0, 27.0, 73.0, 0.0, 45.0, 61.0, 71.0, 88.0),
  Array(57.0, 49.0, 60.0, 51.0, 43.0, 45.0, 0.0, 85.0, 88.0, 115.0),
  Array( 32.0, 64.0, 44.0, 81.0, 56.0, 61.0, 85.0, 0.0, 74.0, 43.0),
  Array( 89.0, 63.0, 71.0, 48.0, 104.0, 71.0, 88.0, 74.0, 0.0, 38.0),
  Array(73.0, 84.0, 73.0, 80.0, 76.0, 88.0, 115.0, 43.0, 38.0, 0.0)
)
println(tsp_simple(distances4))
println(tsp(distances4))