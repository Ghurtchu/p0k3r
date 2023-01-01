package domain

import domain.HasCards.{Hand, Player}

sealed trait HandValue {
  def value: Int
}

object HandValue {

  val range: Range = 1 to 14
  val slidedRanges: List[Seq[Int]] = range.sliding(5, 1).toList
  val straightRanges: List[Seq[Int]] = Seq(10, 11, 12, 13, 14) :: slidedRanges

  implicit val cardOrdering: Ordering[Card] = _.rankValue - _.rankValue

  def apply(hand: Hand, player: Player): HandValue =
    if (isStraightFlush(hand, player)) StraightFlush
    else if (isFourOfAKind(hand, player)) FourOfAKind
    else if (isFullHouse(hand, player)) FullHouse
    else if (isFlush(hand, player)) Flush
    else if (isStraight(hand, player)) Straight
    else if (isThreeOfAKind(hand, player)) ThreeOfAKind
    else if (isTwoPairs(hand, player)) TwoPairs
    else if (isPair(hand, player)) Pair
    else HighCard

  private[domain] def isStraightFlush(hand: Hand, player: Player): Boolean = {
    val (first, second) = (player.cards.head, player.cards.last)
    val (firstColor, secondColor) = (first.color, second.color)
    lazy val firstCaseExists = hand.cards.combinations(3).toList.exists { cards =>
      val allCards = (player.cards ::: cards).sorted
      val allRankValues = allCards.map(_.rankValue)
      val color = allCards.head.color

      straightRanges.contains(allRankValues) && allCards.forall(_.color == color)
    }
    lazy val secondCaseExists = hand.cards.combinations(4).toList.exists { cards =>
      val (cardsWithFirst, cardsWithSecond) = ((first :: cards).sorted, (second :: cards).sorted)
      val (ranksWithFirst, ranksWithSecond) = (cardsWithFirst.map(_.rankValue), cardsWithSecond.map(_.rankValue))

      (straightRanges.contains(ranksWithFirst) && cardsWithFirst.forall(_.color == firstColor)) ||
        (straightRanges.contains(ranksWithSecond) && cardsWithSecond.forall(_.color == secondColor))
    }

    if (firstCaseExists) firstCaseExists else secondCaseExists
  }

  private[domain] def isFourOfAKind(hand: Hand, player: Player): Boolean = {
    val playerRanks = player.cards.map(_.rank)
    val (first, second) = (playerRanks.head, playerRanks.last)
    if (first == second) hand.cards.count(_.rank == first) == 2
    else hand.cards.count(_.rank == first) == 3 ||
      hand.cards.count(_.rank == second) == 3
  }

  private[domain] def isFullHouse(hand: Hand, player: Player): Boolean = {
    val playerRanks = player.cards.map(_.rank)
    val (first, second) = (playerRanks.head, playerRanks.last)
    if (first == second) hand.cards.map(_.rank).toSet.size <= 3
    else {
      (hand.cards.count(_.rank == first) >= 1 && hand.cards.count(_.rank == second) >= 2) ||
        (hand.cards.count(_.rank == first) >= 2 && hand.cards.count(_.rank == second) >= 1)
    }
  }

  private[domain] def isFlush(hand: Hand, player: Player): Boolean = {
    val playerColors = player.cards.map(_.color)
    val (first, second) = (playerColors.head, playerColors.last)
    if (first == second) hand.cards.count(_.color == first) >= 3
    else hand.cards.count(_.color == first) >= 4 ||
      hand.cards.count(_.color == second) >= 4
  }

  private[domain] def isStraight(hand: Hand, player: Player): Boolean = {
    val (first, second) = (player.cards.head, player.cards.last)
    lazy val firstCaseExists = hand.cards.combinations(3).toList.exists { cards =>
      val allCards = (player.cards ::: cards).sorted
      val allRankValues = allCards.map(_.rankValue)

      straightRanges contains allRankValues
    }
    lazy val secondCaseExists = hand.cards.combinations(4).toList.exists { cards =>
      val (cardsWithFirst, cardsWithSecond) = ((first :: cards).sorted, (second :: cards).sorted)
      val (ranksWithFirst, ranksWithSecond) = (cardsWithFirst.map(_.rankValue), cardsWithSecond.map(_.rankValue))

      (straightRanges contains ranksWithFirst) ||
        (straightRanges contains ranksWithSecond)
    }

    if (firstCaseExists) firstCaseExists else secondCaseExists
  }

  private[domain] def isThreeOfAKind(hand: Hand, player: Player): Boolean = {
    val playerRanks = player.cards.map(_.rank)
    val (first, second) = (playerRanks.head, playerRanks.last)
    if (first == second) hand.cards.count(_.rank == first) == 1
    else hand.cards.count(_.rank == first) == 2 ||
      hand.cards.count(_.rank == second) == 2
  }

  private[domain] def isTwoPairs(hand: Hand, player: Player): Boolean = {
    val (first, second) = (player.cards.head, player.cards.last)

    first.rank != second.rank && hand.cards.count(_.rank == first.rank) == 1 &&
      hand.cards.count(_.rank == second.rank) == 1
  }

  private[domain] def isPair(hand: Hand, player: Player): Boolean = {
    player.cards.head.rank == player.cards.last.rank || {
      val (first, second) = (player.cards.head, player.cards.last)

      hand.cards.count(_.rank == first.rank) == 1 ||
        hand.cards.count(_.rank == second.rank) == 1
    }
  }

  case object StraightFlush extends HandValue { override def value: Int = 900 }
  case object FourOfAKind   extends HandValue { override def value: Int = 800 }
  case object FullHouse     extends HandValue { override def value: Int = 700 }
  case object Flush         extends HandValue { override def value: Int = 600 }
  case object Straight      extends HandValue { override def value: Int = 500 }
  case object ThreeOfAKind  extends HandValue { override def value: Int = 400 }
  case object TwoPairs      extends HandValue { override def value: Int = 300 }
  case object Pair          extends HandValue { override def value: Int = 200 }
  case object HighCard      extends HandValue { override def value: Int = 100 }
}

