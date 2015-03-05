import Crosswords._

object Main extends App {

  val startTime = System.currentTimeMillis

  val input = Set(
    "kuracjusz", "traktat", "meduza", "klej", "pożar", "głód", "toga"//, "zakała"
    //"kawa"//, "matero"//, "pokuta", "wielogłos", "drzewo", "skarb", "eter"
//    "topielec", "korzyść", "wykład", "kufel", "pakistan", "wołga", "aksamit",
//    "katamaran", "mamałyga", "kukułka", "pumpernikiel", "skafander"
  )

  val stream = generate(input) take 5 sortBy (_.rate) take 3
  stream foreach { cw => println(s"------\n$cw") }

  val endTime = System.currentTimeMillis
  println("Total execution time: " + (endTime-startTime) + "ms")

}
