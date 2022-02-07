case class MarkovChain(start: Array[Double],
                       transition: Array[Array[Double]],
                       emit: Array[Array[Double]],
                       emitNames: Array[String]) {

  def stateNameToNumber(name: String): Int = {
    //FIXME throw exception if state name is unknown
    emitNames.search(name).insertionPoint
  }

  def viterbi(a: Array[String]): Array[Int] = {
    viterbi(a.map(name => stateNameToNumber(name)))
  }

  // apply Viterbi algorithm on given state sequence
  def viterbi(a: Array[Int]): Array[Int] = {
    val k_max = a.length
    val z_max = transition.length
    val t = Array.ofDim[(Double, List[Int])](k_max, z_max)

    for (k <- 0 until k_max)
      for(z <- 0 until z_max)
        if (k == 0)
          t(k)(z) = (emit(z)(a(k)) * start(z), List(z))
        else {
          val (p_z_previous_max, state_sequence) = (0 until z_max)
            .map(z_prev => (transition(z_prev)(z) * t(k-1)(z_prev)._1, z :: t(k-1)(z_prev)._2))
            .maxBy(_._1)
          // store probability and sequence of states up to this state
          t(k)(z) = (emit(z)(a(k)) * p_z_previous_max, state_sequence)
        }

    t.foreach(x => println(x.mkString(", ")))
    // return sequence with highest probability, sequence has to be reversed because elements were added at front
    t(k_max - 1).maxBy(_._1)._2.toArray.reverse
  }
}

//  start with state fair, unfair
val coinStart = Array(0.9, 0.1)
val coinTransition = Array(
  // to fair, unfair
  Array(0.9, 0.1), // from state fair
  Array(0.1, 0.9) // from state unfair
)
val coinEmit = Array(
  //emit head, tail
  Array(0.5, 0.5), // at state fair
  Array(0.9, 0.1) // at state unfair
)
val coinEmitNames = Array(
  "h", // head
  "t" // tail
)
val unfairCoin = MarkovChain(coinStart, coinTransition, coinEmit, coinEmitNames)
unfairCoin.viterbi(Array(0, 1, 1, 0, 0, 1, 0, 0, 0, 0, 0, 0))
unfairCoin.viterbi(Array("h", "t", "t", "h", "h", "t", "h", "h", "h", "h", "h", "h"))
unfairCoin.viterbi(Array("h", "t", "t", "h", "h", "t", "h", "h", "h", "h", "t", "t"))