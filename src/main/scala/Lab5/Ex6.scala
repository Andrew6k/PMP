package Lab5

import com.cra.figaro.algorithm.sampling.Importance
import com.cra.figaro.language.{Apply, Constant, Element, Flip}
import com.cra.figaro.library.compound.If

object Ex6
{
	def tennis(probP1ServeWin: Double, probP1Winner: Double, probP1Out: Double, probP1Net: Double, probP2ServeWin: Double, probP2Winner: Double, probP2Out: Double, probP2Net: Double, probArbitraj: Double): Element[Boolean] = 
	{
		//modelare serva
		def rally(firstShot: Boolean, player1: Boolean): Element[Boolean] = 
		{
			//selectare winner in functie de cine serveste
			val pWinner = if (firstShot && player1) probP1ServeWin
							else if (firstShot && !player1) probP2ServeWin
								else if (player1) probP1Winner
										else probP2Winner
			//eroare out
			val pErrorOut = if (player1) probP1Out else probP2Out

			//eroare lovire fileu
			val pErrorNet = if (player1) probP1Net else probP2Net

			//realizare flip pentru cele 3 variabile
			val winner = Flip(pWinner)
			val errorOut = Flip(pErrorOut)
			val errorNet = Flip(pErrorNet)

			//greseala arbitru
			val arbitru = Flip(probArbitraj)

			//selectare castigator sau continuare
			//daca nu castiga jucatorul 1, verificam errorOut daca este true castiga jucator2
			//daca este false, verificam errorNet daca este true castiga jucator 2
			//altfel continua
			//adaugam si probabilitatea ca arbitrul sa greseasca, astfel ca daca greseste si considera out la winner
			//castiga celalalt jucator; daca mingea este in out si arbitru considera ca este corecta, castiga celalalt
			//jucator
			If(winner,
				If(arbitru,
					Constant(!player1),
					Constant(player1)
				),
				If(errorOut,
					If(arbitru,
						Constant(player1), 
						Constant(!player1)
					), 
					If(errorNet,
						Constant(!player1),
						rally(false, !player1)
					)
				)
			)
		}

		def game(p1Serves: Boolean, p1Points: Element[Int], p2Points: Element[Int]): Element[Boolean] =
		{
			//puncte obtinute de jucator
			val p1WinsPoint = rally(true, p1Serves)

			//adaugare puncte p1
			val newP1Points = Apply(p1WinsPoint, p1Points,
								(wins: Boolean, points: Int) =>
									if (wins) points + 1 else points
			)

			//adaugare puncte p2
			val newP2Points = Apply(p1WinsPoint, p2Points,
								(wins: Boolean, points: Int) =>
									if (wins) points else points + 1
			)

			//verificare terminare set p1 castiga
			val p1WinsGame = Apply(newP1Points, newP2Points,
								(p1: Int, p2: Int) =>
									p1 >= 4 && p1 - p2 >= 2
			)

			//verificare terminare set p2 castiga
			val p2WinsGame = Apply(newP2Points, newP1Points,
								(p2: Int, p1: Int) =>
									p2 >= 4 && p2 - p1 >= 2
			)

			//daca a castigat unul dintre jucatori
			val gameOver = p1WinsGame || p2WinsGame

			If(gameOver, 
				p1WinsGame, 
				game(p1Serves, newP1Points, newP2Points)
			)
		}

		def play(p1Serves: Boolean, p1Sets: Element[Int], p2Sets: Element[Int],
					p1Games: Element[Int], p2Games: Element[Int]): Element[Boolean] =
		{
			//calculare seturi castigate jucator
			val p1WinsGame = game(p1Serves, Constant(0), Constant(0))
			
			//castigator set p1
			val newP1Games = Apply(p1WinsGame, p1Games, p2Games,
								(wins: Boolean, p1: Int, p2: Int) =>
									if (wins)
									{
										if (p1 >= 5) 0 else p1 + 1
									}
									else
									{
										if (p2 >= 5) 0 else p1
									}
			)

			//castigator set p2
			val newP2Games = Apply(p1WinsGame, p1Games, p2Games,
								(wins: Boolean, p1: Int, p2: Int) =>
									if (wins)
									{
										if (p1 >= 5) 0 else p2
									}
									else
									{
										if (p2 >= 5) 0 else p2 + 1
									}
			)

			//incrementare set
			val newP1Sets = Apply(p1WinsGame, p1Games, p1Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (wins && games == 5)
										sets + 1
									else 
										sets
			)

			val newP2Sets = Apply(p1WinsGame, p2Games, p2Sets,
								(wins: Boolean, games: Int, sets: Int) =>
									if (!wins && games == 5)
										sets + 1
									else
										sets
			)

			//terminare joc
			val matchOver = Apply(newP1Sets, newP2Sets,
								(p1: Int, p2: Int) =>
									p1 >= 2 || p2 >= 2
			)
			
			//meci incheiat => castigator, altfel continuare
			If(matchOver,
				Apply(newP1Sets, (sets: Int) => sets >= 2),
				play(!p1Serves, newP1Sets, newP2Sets, newP1Games, newP2Games)
			)
		}

		play(true, Constant(0), Constant(0), Constant(0), Constant(0))
	}


	def main(args: Array[String])
	{
		//primul jucator are prob mai mare sa scoate mingea din joc, al doilea are prob mai mare sa loveasca fileul
		//val tennis_match = tennis(0.5, 0.5, 0.3, 0.2, 0.5, 0.5, 0.2, 0.4, 0.02)

		//al doilea jucator are prob mai mare in ambele cazuri sa greaseasca
		//val tennis_match = tennis(0.5, 0.5, 0.1, 0.2, 0.5, 0.5, 0.5, 0.4, 0.02)

		//crestem prob ca arbitrul sa greseasca, scade prob ca player1 sa castige, pt aceleasi prob ca in ex anterior
		val tennis_match = tennis(0.5, 0.5, 0.1, 0.2, 0.5, 0.5, 0.5, 0.4, 0.08)
		
		val alg = Importance(200, tennis_match)
		alg.start()
		alg.stop()
		println("Expected gain:" + alg.probability(tennis_match, true))
	}
}