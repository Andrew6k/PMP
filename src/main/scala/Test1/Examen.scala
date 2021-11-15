package Test1

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}
import com.cra.figaro.algorithm.sampling.Importance

object Ex {

	def play(p1Wins: Element[Int], p2Wins:Element[Int], no:Int): Element[Int] ={
		val die1 = FromRange(1, 7)
		val die2 = FromRange(1, 7)
		val player = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2 )

		
		val winner = RichCPD(player,
            OneOf(7, 11) -> Constant("player1"),
            OneOf(2,3,12) -> Constant("player2"),
            * -> Constant("tie") //egal
        )

		if(winner==Constant("player1")){
			p1Wins = p1Wins + 1
			no = no - 1
		}
		else if(winner==Constant("player2")){
			p2Wins = p2Wins + 1
			no = no - 1
		}
		if(no == 0)
			if(p1Wins > p2Wins)
				1
			else if(p1Wins < p2Wins)
				2
			else
				0
        else play(p1Wins,p2Wins,no)
	}
	def main(args: Array[String]) {
		val die1 = FromRange(1, 7)
		val die2 = FromRange(1, 7)
		val player = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2 )
		
		val winner = RichCPD(player,
            OneOf(7, 11) -> Constant("player1"),
            OneOf(2,3,12) -> Constant("player2"),
            * -> Constant("tie") //egal
        )
        
		println(VariableElimination.probability(winner, "player1")) //castiga primul jucator
		println(VariableElimination.probability(winner,"player2")) //castiga al doilea jucator

		//4. Probabilitatea ca jucatorul 1 sa castige este mai mare, deoarece exista mai multe combinatii
		// ca suma sa dea 7,11 fata de combinatiile ca suma sa dea 2,3,12

		val result = play(0,0,10)
		val alg= Importance(200, result)
		alg.start()
		alg.stop()
		println(alg.probability(result,1))
		println(alg.probability(result,2))

		//Sansa ca sa fie tie fiind mult mai mare decat celelalte, resultatele sunt apropiate
	}
}