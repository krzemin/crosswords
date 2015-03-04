import scala.collection.mutable

case class Point(x: Int, y: Int)

sealed trait Orientation
case object Horizontal extends Orientation
case object Vertical extends Orientation

case class WordInside(text: String,
                      orientation: Orientation)

case class Crossword(matrix: Map[Point, WordInside] = Map.empty,
                     charIdx: Map[Char, Set[Point]] = Map.empty) {

  def width: Int = ???

  def height: Int = ???

  def area: Int = width * height

  def diffSide: Int = math.abs(width - height)

  def rate: Double = area * (1 + diffSide)

  def next(word: String): List[Crossword] = ???

  def toMatrix: List[String] = ???

  override def toString = toMatrix.mkString("\n")

}

implicit val cwOrd: Ordering[Crossword] = Ordering.by(_.rate)

case class State(crossword: Crossword, wordsLeft: Set[String])

implicit val stOrd: Ordering[State] = Ordering.by(s => s.crossword.rate * s.wordsLeft.size)


def generate(words: Set[String]): Stream[Crossword] = {

  val initialStates = words.toList.sortBy(-_.length).map { word =>
    State(Crossword().next(word).head, words - word)
  }.sorted


  ???
}






