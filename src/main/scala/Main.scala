import Crosswords._

object Main extends App {

  val startTime = System.currentTimeMillis

  val input = Set("lekarz", "palacz", "wojsko", "klinika", "świnia", "kołyma")

  val stream = generate(input) take 3
  stream foreach { cw => println(s"------\n$cw") }

  val endTime = System.currentTimeMillis
  println("Total execution time: " + (endTime-startTime) + "ms");

}
