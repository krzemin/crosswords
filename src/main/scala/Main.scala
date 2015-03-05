import Crosswords._

object Main extends App {

  val input = Set("dupa", "kupa", "lekarz", "palacz")
  generate(input) foreach { cw => println(s"------\n$cw") }


}
