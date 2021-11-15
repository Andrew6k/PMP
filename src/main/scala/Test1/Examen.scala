package Test1

import com.cra.figaro.language._
import com.cra.figaro.library.atomic.discrete._
import com.cra.figaro.algorithm.factored.VariableElimination
import com.cra.figaro.library.compound.{RichCPD, OneOf, *}

object Ex {
	def main(args: Array[String]) {
		val die1 = FromRange(1, 7)
		val die2 = FromRange(1, 7)
		val player = Apply(die1, die2, (i1: Int, i2: Int) => i1 + i2 )
		
		
		val winner = RichCPD(player,
            OneOf(7, 11) ->1,
            OneOf(2,3,12) -> 2,
            * -> 3
        )
        
		println(VariableElimination.probability(winner, 1))
	}
}