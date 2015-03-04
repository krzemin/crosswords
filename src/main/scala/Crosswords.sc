import scala.annotation.tailrec
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

case class State(crossword: Crossword, wordsLeft: Set[String]) {

  def isFinal: Boolean = wordsLeft.isEmpty
}

implicit val stOrd: Ordering[State] = Ordering.by(s => s.crossword.rate * s.wordsLeft.size)


def generate(words: Set[String]): Stream[Crossword] = {

  def genAux(pq: mutable.PriorityQueue[State]): Stream[Crossword] = pq.isEmpty match {
    case true => Stream.empty
    case false =>
      val curState = pq.dequeue()
      curState.isFinal match {
        case true => curState.crossword #:: genAux(pq)
        case false =>
          val nextStates = curState.wordsLeft.toList.flatMap { word =>
            curState.crossword.next(word).map { crossword =>
              State(crossword, curState.wordsLeft - word)
            }
          }
          pq.enqueue(nextStates : _*)
          genAux(pq)
      }
  }

  val initialStates = words.toList.sortBy(-_.length).map { word =>
    State(Crossword().next(word).head, words - word)
  }.sorted

  genAux(mutable.PriorityQueue(initialStates : _*))
}






