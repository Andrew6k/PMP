package Lab10

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.continuous.Normal
import com.cra.figaro.library.atomic.discrete.{FromRange, Poisson}
import com.cra.figaro.library.compound.{If, ^^}
import com.cra.figaro.algorithm.filtering.ParticleFilter

object Ex5b {
  val months = 12

  val initial = Universe.createNew()
  Constant(1500)("capital", initial)
  Constant(0)("investment", initial)
  Constant(0)("profit", initial)

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


  def nextUniverse(previous: Universe): Universe = {
    val next = Universe.createNew()
    val previousCapital = previous.get[Int]("capital")
    val previousInvestment = previous.get[Int]("investment")
    val previousProfit = previous.get[Int]("profit")
    val newState = Chain(previousCapital, previousInvestment, previousProfit, transition _)
    Apply(newState, (s: (Int, Int, Int)) => s._1)("capital", next)
    Apply(newState, (s: (Int, Int, Int)) => s._2)("investment", next)
    Apply(newState, (s: (Int, Int, Int)) => s._3)("profit", next)
    next
  }

  def main(args: Array[String]) {
    val alg = ParticleFilter(initial, nextUniverse, 10000)
    alg.start()
    for { time <- 1 to 12 } {
      alg.advanceTime()
			println("expected profit = " + alg.currentExpectation("profit", (p: Double) => p))
			println("expected investment = " + alg.currentExpectation("investment", (p: Double) => p))
			println("expected capital = " + alg.currentExpectation("capital", (p: Double) => p))
    }
  }
}