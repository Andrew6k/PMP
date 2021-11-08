package Lab6

import com.cra.figaro.language._
import com.cra.figaro.algorithm.sampling._
import com.cra.figaro.library.compound.If
import scala.collection.mutable.ListBuffer

object Ex1 {
	def golf(player: Double){
		val pairs= List.tabulate(18)(n => 2+n%5)
		//println(pairs)
		val shots_per_hole = new ListBuffer[Int]()
		for(i <- 0 to 18)
		{
			val s = Flip(player)
			val s1= Flip(player/2)
			val s2= Flip(player/8)
			val s3= Flip(4/5 * (1-13/8*player))
			val s4= Flip(1/5 * (1-13/8*player))
			val shots=If(s,
				pairs(i),
				If(s1,
					pairs(i)-1,
					If(s2,
						pairs(i)-2,
						If(s3,
							pairs(i)+1,
							If(s4,
								pairs(i)+2),
								0))))
			//val shots_per_hole = pairs.chain((i : Int) => If(s), )
			shots_per_hole +=(Int)shots 
		}
		println(shots_per_hole)
		var score = 0
		for(i <-0 to 18){
			score += (pairs(i) - shots_per_hole(i))
		}
	}
	def main(args: Array[String]) {
		val golf_match= golf(0.3)
		val alg = Importance(200, golf_match)
		alg.start()
		alg.stop()
		println(alg.probability(golf_match, 80))
	}
}