import HandValue.ThreeOfAKind
import HasCards._
import scala.collection.immutable.{ListMap, TreeMap}
import HandValue._

final case class Table(hand: Hand, players: List[Player]) {

  implicit val ordering: Ordering[Player] = _.rankValue - _.rankValue

  def evaluateHand: Map[Int, List[Player]] = ListMap.from {
    players.map { player =>
      HandValue(hand, player) match {
        case HandValue.StraightFlush => getStraightFlush(hand, player)
        case HandValue.FourOfAKind   => getFourOfAKind(hand, player)
        case HandValue.FullHouse     => getFullHouse(hand, player)
        case HandValue.Flush         => getFlush(hand, player)
        case HandValue.Straight      => getStraight(hand, player)
        case HandValue.ThreeOfAKind  => getThreeOfAKind(hand, player)
        case HandValue.TwoPairs      => getTwoPairs(hand, player)
        case HandValue.Pair          => getPair(hand, player)
        case HandValue.HighCard      => getHighCard(player)
      }
    }.groupBy(_.rankValue).toSeq.sortBy(_._1).reverse
  }

  private def getStraightFlush(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val allCards = hand.cards ::: player.cards
    val straightRank = allCards.combinations(5).flatMap { cards =>
      val sorted = cards.sorted
      val sortedRanks = sorted.map(_.rank)
      if (straightRanges.contains(sortedRanks) && sorted.forall(c => c.color == first.color || c.color == second.color)) Some(sortedRanks.map(_.value).sum) else None
    }.toList.max

    player.copy(handValue = Some(Straight), rankValue = straightRank)
  }

  private def getFourOfAKind(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = 4 * (if (first == second || hand.cards.count(_ == first) == 3) first.rankValue else second.rankValue)

    player.copy(handValue = Some(FourOfAKind), rankValue = rankValue)
  }

  private def getFullHouse(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = if (first == second) {
      val others = hand.cards.filter(c => hand.cards.count(_ == c) == 3).map(_.rankValue).sum

      first.rankValue + second.rankValue + others
    } else if (hand.cards.count(_ == first) == 2) first.rankValue * 3 + second.rankValue * 2
    else first.rankValue * 2 + second.rankValue * 3

    player.copy(handValue = Some(FullHouse), rankValue = rankValue)
  }

  private def getFlush(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = if (first.color == second.color) filterByColorAndSumN(hand, first.color, 3) + first.rankValue + second.rankValue
    else if (hand.cards.count(_.color == first.color) >= 4) filterByColorAndSumN(hand, first.color, 4) + first.rankValue
    else filterByColorAndSumN(hand, second.color, 4) + second.rankValue

    player.copy(handValue = Some(Flush), rankValue = rankValue)
  }

  private def filterByColorAndSumN(hand: Hand, colors: Color, n: Int): Int =
    hand.cards.filter(_.color == colors).sorted.reverse.take(n).map(_.rankValue).sum

  private def getStraight(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val allCards = hand.cards ::: player.cards
    val straightRank = allCards.combinations(5).flatMap { cards =>
      val sortedRanks = cards.sorted.map(_.rankValue)
      if (straightRanges.contains(sortedRanks)) Some(sortedRanks.sum + (first.rankValue + second.rankValue)) else None
    }.toList.max

    player.copy(handValue = Some(Straight), rankValue = straightRank)
  }

  private def getThreeOfAKind(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = 3 * (if (first == second || hand.cards.count(_ == first) == 2) first.rankValue else second.rankValue)

    player.copy(handValue = Some(ThreeOfAKind), rankValue = rankValue)
  }

  private def getTwoPairs(hand: Hand, player: Player): Player = {
    implicit val rankOrdering: Ordering[Rank] = _.value - _.value
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = 2 * (if (first == second) {
      val possibleCards = TreeMap.from(hand.cards.groupBy(_.rank).filter { case (_, value) => value.size > 1 })
      val secondRankValue = possibleCards.head._2.head.rankValue

      first.rankValue + secondRankValue
    } else first.rankValue + second.rankValue)

    player.copy(handValue = Some(TwoPairs), rankValue = rankValue)
  }

  private def getPair(hand: Hand, player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val isFirstPair = hand.cards.count(_ == first) == 1
    val rankValue = 2 * (if (isFirstPair) first else second).rankValue

    player.copy(handValue = Some(Pair), rankValue = rankValue)
  }

  private def getHighCard(player: Player): Player = {
    val (first, second) = (player.cards.head, player.cards.last)
    val rankValue = (if (first.rank > second.rank) first else second).rankValue

    player.copy(handValue = Some(HighCard), rankValue = rankValue)
  }

}
