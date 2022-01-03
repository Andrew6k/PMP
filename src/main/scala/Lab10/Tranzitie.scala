package Lab10

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.discrete.{FromRange, Poisson}
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.sampling.Importance

object Ex5a {
  
    val months = 13
    val fraction = 0.3

    var investment: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var profit: Array[Element[Double]] = Array.fill(months)(Constant(0.0))
    var capital: Array[Element[Double]] = Array.fill(months)(Constant(0.0))

    capital(0) = Constant(1200.0)

  def transition(capital: Int, investment: Int, profit: Int): (Element[(Int, Int, Int)]) = {
    
    val newinvestment = Apply(capital, (cap: Double) => cap * fraction)
   
    val newprofit = Chain(newinvestment, capital, (inv: Double, cap: Double) =>
        if (inv >= 0.5 * cap) Select(0.1 -> (0.4 * cap), 0.3 -> (0.5 * cap), 0.6 -> (0.7 * cap));
        else if (inv >= 0.3 * cap) Select(0.2 -> (0.25 * cap), 0.6 -> (0.5 * cap), 0.2 -> (0.35 * cap));
        else Select(0.6 -> (0.3 * cap), 0.3 -> (0.2 * cap), 0.1 -> (0.1 * cap)))
    
    val newlycapital = Apply(newprofit, capital, newinvestment,
        (prof: Double, cap: Double, invest: Double) => cap + prof - invest)
    
    ^^(newlycapital, newinvestment, newprofit)
  }

  for { step <- 1 until months } {
    val newState =
      Chain(capital(step - 1), investment(step - 1), profit(step-1), (l: Int, i: Int, j: Int) => transition(l, i, j))
    capital(step) = newState._1
    investment(step) = newState._2
    profit(step) = newState._3
  }

  def main(args: Array[String]) {
    val alg = Importance(10000, profit(months - 1))
    alg.start()
    println(alg.probability(profit(months - 1), (i: Int) => i > 600))
  }
}