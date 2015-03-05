import scala.collection.mutable

object Crosswords {

  case class Point(x: Int, y: Int)

  sealed trait Orientation {
    def opposite: Orientation
    def plus(p: Point, d: Int): Point
    def nextPoint(p: Point): Point = plus(p, 1)
    def zipWithPoints(word: String, starting: Point): Seq[(Char, Point)] = {
      word.tail.scanLeft( (word.head, starting) ) { case ((_, pt), c) =>
        (c, nextPoint(pt))
      }
    }
  }

  case object Horizontal extends Orientation {
    def opposite: Orientation = Vertical
    def plus(p: Point, d: Int): Point = Point(p.x + d, p.y)
  }

  case object Vertical extends Orientation {
    def opposite: Orientation = Horizontal
    def plus(p: Point, d: Int): Point = Point(p.x, p.y + d)
  }


  case class WordInside(text: String,
                        orientation: Orientation)


  case class Crossword(matrix: Map[Point, Set[WordInside]] = Map.empty,
                       charIdx: Map[Char, Set[Point]] = Map.empty,
                       hWordStartIdx: Map[Point, Point] = Map.empty,
                       vWordStartIdx: Map[Point, Point] = Map.empty,
                       boundRect: (Point, Point) = (Point(0, 0), Point(0, 0))) {
    def width: Int = boundRect._2.x - boundRect._1.x

    def height: Int = boundRect._2.y - boundRect._1.y

    def area: Int = width * height

    def diffSide: Int = math.abs(width - height)

    def rate: Double = area * (1 + diffSide) * (1 + diffSide)

    def letterMatch(orientation: Orientation)(letterAndPoint: (Char, Point)): Boolean = {
      val (letter, point) = letterAndPoint
      val letterAtPoint = toCharMap.getOrElse(point, ' ')
      lazy val oppOrientation = orientation.opposite
      lazy val (nearPoint1, nearPoint2) = (oppOrientation.plus(point, -1), oppOrientation.nextPoint(point))
      lazy val (np1Char, np2Char) = (toCharMap.getOrElse(nearPoint1, ' '), toCharMap.getOrElse(nearPoint2, ' '))
      lazy val oppWordIdx = oppOrientation match { case Horizontal => hWordStartIdx ; case Vertical => vWordStartIdx }
      lazy val sameWordIdx = orientation match { case Horizontal => hWordStartIdx ; case Vertical => vWordStartIdx }

      lazy val np1Ok = np1Char == ' ' || (sameWordIdx.get(nearPoint1).isEmpty &&
        oppWordIdx.get(nearPoint1).exists { wordStartPt =>
          val wordLen = matrix(wordStartPt).find(_.orientation == oppOrientation).get.text.length
          val wordEndPt = oppOrientation.plus(wordStartPt, wordLen - 1)
          nearPoint1.x < wordEndPt.x || nearPoint1.y < wordEndPt.y
        })
      lazy val np2Ok = np2Char == ' ' || (sameWordIdx.get(nearPoint2).isEmpty &&
        oppWordIdx.get(nearPoint2).exists(_ != nearPoint2))
      (letterAtPoint == letter || letterAtPoint == ' ') && np1Ok && np2Ok
    }

    def tryInsert(word: String, orientation: Orientation, startingPoint: Point): Option[Crossword] = {
      val charWithPoints = orientation.zipWithPoints(word, startingPoint)

      val canInsert: Boolean = {
        charWithPoints.forall(letterMatch(orientation)) &&
          !toCharMap.contains(orientation.plus(startingPoint, -1)) &&
          !toCharMap.contains(orientation.plus(startingPoint, word.length))
      }

      if (canInsert) {
        val endingPoint = orientation.plus(startingPoint, word.length)
        val addedCharIdx = charWithPoints.groupBy(_._1).mapValues(_.map(_._2).toSet)
        val newHWordStartIdx = if(orientation == Horizontal) wordStartIdx(startingPoint, charWithPoints) else Map.empty[Point,Point]
        val newVWordStartIdx = if(orientation == Vertical) wordStartIdx(startingPoint, charWithPoints) else Map.empty[Point,Point]
        Some(Crossword(
          matrix = this.matrix + (startingPoint -> (this.matrix.getOrElse(startingPoint, Set.empty) + WordInside(word, orientation))),
          charIdx = addedCharIdx.foldLeft(this.charIdx) { case (cIdx, (c, pointSet)) =>
            cIdx + (c -> (pointSet ++ cIdx.getOrElse(c, Set.empty[Point])))
          },
          hWordStartIdx = this.hWordStartIdx ++ newHWordStartIdx,
          vWordStartIdx = this.vWordStartIdx ++ newVWordStartIdx,
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

    def wordStartIdx(startingPoint: Point, charsWithPoints: Seq[(Char, Point)]): Map[Point, Point] =
      charsWithPoints.map(_._2 -> startingPoint).toMap

    def next(word: String): Seq[Crossword] = if(matrix.isEmpty) {
      val startingPoint = Point(0, 0)
      val withPoints = Horizontal.zipWithPoints(word, startingPoint)
      Seq(Crossword(
        matrix = Map(startingPoint -> Set(WordInside(word, Horizontal))),
        charIdx = withPoints.groupBy(_._1).mapValues(_.map(_._2).toSet),
        hWordStartIdx = wordStartIdx(startingPoint, withPoints),
        boundRect = (startingPoint, Point(word.length, 1))
      ))
    } else {
      val possibleCrossingPoints = word.flatMap { c =>
        charIdx.getOrElse(c, Set.empty[Point]).map(p => (p, c))
      }
      val wordCharIdx = word.zipWithIndex.groupBy(_._1).mapValues(_.map(_._2))
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
      case (map, (point, words)) =>
        words.foldLeft(map) { case (map2, word) =>
          map2 ++ word.orientation.zipWithPoints(word.text, point).map(_.swap)
        }
    }

    def toMatrix: Seq[String] = {
      val buff: Array[StringBuilder] = Array.fill(height) {
        new StringBuilder(width, " " * width)
      }

      toCharMap foreach { case (Point(x, y), c) =>
        buff(y - boundRect._1.y)(x - boundRect._1.x) = c
      }

      buff.map(_.toString()).toList
    }

    def infoLine = s"${width}x$height|r:$rate|l:${matrix.size}|tl:${boundRect._1}"

    override def toString = infoLine + "\n" + toMatrix.mkString("\n")
  }

  case class State(crossword: Crossword, wordsLeft: Set[String]) {
    def isFinal: Boolean = wordsLeft.isEmpty
  }

  implicit val stOrd: Ordering[State] = Ordering.by(s => math.sqrt(s.crossword.rate) * s.wordsLeft.size)


  def generate(words: Set[String]): Stream[Crossword] = {

    val initialStates = words.toList.map { word =>
      State(Crossword().next(word).head, words - word)
    }.sorted

    val pq = mutable.PriorityQueue(initialStates : _*)

    Stream.from(1).flatMap { _ =>
      var curState: State = null
      while(pq.nonEmpty && (curState == null || !curState.isFinal)) {
        curState = pq.dequeue()
        val nextStates = curState.wordsLeft.toList.flatMap { word =>
          curState.crossword.next(word).map { crossword =>
            State(crossword, curState.wordsLeft - word)
          }
        }
        pq.enqueue(nextStates: _*)
      }
      if(curState == null || !curState.isFinal) {
        None
      } else {
        Some(curState.crossword)
      }
    }
  }

}
