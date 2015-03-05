import scala.collection.mutable
case class Point(x: Int, y: Int)
sealed trait Orientation {
  def plus(p: Point, d: Int): Point
  def nextPoint(p: Point): Point = plus(p, 1)
  def zipWithPoints(word: String, starting: Point): List[(Char, Point)] = {
    word.tail.scanLeft( (word.head, starting) ) { case ((_, pt), c) =>
      (c, nextPoint(pt))
    }.toList
  }
}
case object Horizontal extends Orientation {
  def plus(p: Point, d: Int): Point = Point(p.x + d, p.y)
}
case object Vertical extends Orientation {
  def plus(p: Point, d: Int): Point = Point(p.x, p.y + d)
}
case class WordInside(text: String,
                      orientation: Orientation)
case class Crossword(matrix: Map[Point, WordInside] = Map.empty,
                     charIdx: Map[Char, Set[Point]] = Map.empty,
                     boundRect: (Point, Point) = (Point(0, 0), Point(0, 0))) {
  def width: Int = boundRect._2.x - boundRect._1.x

  def height: Int = boundRect._2.y - boundRect._1.y

  def area: Int = width * height

  def diffSide: Int = math.abs(width - height)

  def rate: Double = area * (1 + diffSide)

  def letterMatch(letterAndPoint: (Char, Point)): Boolean = {
    val (letter, point) = letterAndPoint
    val letterAtPoint = toCharMap(point)
    letterAtPoint == letter || letterAtPoint == ' '
  }

  def tryInsert(word: String, orientation: Orientation, startingPoint: Point): Option[Crossword] = {
    val charWithPoints = orientation.zipWithPoints(word, startingPoint)
    if (charWithPoints.forall(letterMatch)) {
      val endingPoint = orientation.plus(startingPoint, word.length)
      val addedCharIdx = charWithPoints.groupBy(_._1).mapValues(_.map(_._2).toSet)
      Some(Crossword(
        matrix = this.matrix + (startingPoint -> WordInside(word, orientation)),
        charIdx = addedCharIdx.foldLeft(this.charIdx) { case (cIdx, (c, pointSet)) =>
            cIdx + (c -> (pointSet ++ cIdx.getOrElse(c, Set.empty[Point])))
        },
        boundRect = (
          Point(math.min(startingPoint.x, boundRect._1.x),
                math.min(startingPoint.y, boundRect._1.y)),
          Point(math.max(endingPoint.x, boundRect._2.x),
                math.max(endingPoint.y, boundRect._2.y))
        )
      ))
    } else {
      None
    }
  }

  def next(word: String): List[Crossword] = if(matrix.isEmpty) {
    List(Crossword(
      matrix = Map(Point(0, 0) -> WordInside(word, Horizontal)),
      charIdx = Horizontal.zipWithPoints(word, Point(0, 0))
        .groupBy(_._1).mapValues(_.map(_._2).toSet),
      boundRect = (Point(0, 0), Point(word.length, 1))
    ))
  } else {
    val possibleCrossingPoints = word.toList.flatMap(c => charIdx(c).map(p => (p, c)))
    val wordCharIdx = word.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2).toList)
    val crossingPointsWithPosition = possibleCrossingPoints.flatMap { case (pt, c) =>
      wordCharIdx(c).map(i => (pt, i))
    }

    val horizontalCombinations = crossingPointsWithPosition.flatMap { case (Point(x, y), i) =>
      tryInsert(word, Horizontal, Point(x - i, y))
    }
    val verticalCombinations = crossingPointsWithPosition.flatMap { case (Point(x, y), i) =>
      tryInsert(word, Vertical, Point(x, y - i))
    }

    horizontalCombinations ++ verticalCombinations
  }



  lazy val toCharMap: Map[Point, Char] = matrix.foldLeft(Map.empty[Point, Char]) {
    case (map, (point, word)) =>
      word.orientation.zipWithPoints(word.text, point).foldLeft(map) { case (m, (c, pt)) =>
        m + (pt -> c)
      }
  }

  def toMatrix: List[String] = {
    val buff: Array[StringBuilder] = Array.fill(height) {
      new StringBuilder(width, " " * width)
    }

    toCharMap foreach { case (Point(x, y), c) =>
      buff(y)(x) = c
    }

    buff.map(_.toString()).toList
  }

  override def toString = toMatrix.mkString("\n")

}
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



// tests ;)
Horizontal.zipWithPoints("katamaran", Point(0, 0))
Vertical.zipWithPoints("katamaran", Point(0, 0))
val cw1 = Crossword().next("katamaran").head
cw1.toCharMap
cw1.toMatrix
cw1.toString

val cw2 = cw1.next("lalka").head
cw2.toCharMap
cw2.toMatrix
cw2.toString
