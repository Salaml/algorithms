// longest common subsequence
def lcs(a: String, b: String):String = {
  val d = Array.ofDim[(Int, String)](a.length+1,b.length+1)

  for(i <- d.indices)
    for(j <- d(i).indices) {
      if(i == 0 || j == 0)
        d(i)(j) = (0, "")
      else {
        val matches =
          if(a(i-1) == b(j-1)) {
            // match -> add char at front
            (d(i-1)(j-1)._1 + 1, a(i-1) + d(i-1)(j-1)._2)
          } else
            d(i-1)(j-1)

        d(i)(j) =
          List(
            matches,
            d(i-1)(j),
            d(i)(j-1)
          ).maxBy(x => x._1)
      }
    }
  d.foreach(x => println(x.mkString("")))
  d(a.length)(b.length)._2.reverse
}

lcs("HORSE", "HOUSE") // (4, "HOSE")

lcs("SEQUENCE", "REDUCE") // (4, "EUCE")

lcs("FOXTROT", "ROMEO") // (2, "RO")
lcs("FOXTROT", "FOMEO") // (3, "FOO")
lcs("FOXTROT", "FROMEO") // (3, "FRO")