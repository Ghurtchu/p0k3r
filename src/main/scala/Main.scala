import Main.{Table, _}
import HasCards._
import Main.HandValue._

import scala.collection.immutable.{ListMap, TreeMap}

object Main {
  def main(args: Array[String]): Unit = {
    val game = Game(args(0))
    game match {
      case Some(_) =>
        val hand = Hand(args(1))
        val players = args.slice(2, args.length).map(Player.apply).toList
        val table = Table(hand, players)
        val playersSorted = table.evaluateHand

        println(s"Hand: ${hand.cards.map(_.value).mkString("(",",", ")")}")

        playersSorted.foreach { case (i, value) =>
          println(s"Rank: $i = $value")
        }
      case None =>
    }
  }

  sealed trait Game

  object Game {

    def apply(game: String): Option[Game] = game match {
      case "texas-holdem"   => Some(TexasHoldem)
      case "omaha-holdem"   => Some(OmahaHoldem)
      case "five-card-draw" => Some(FiveCardDraw)
      case _                => None
    }

    case object TexasHoldem  extends Game
    case object OmahaHoldem  extends Game
    case object FiveCardDraw extends Game
  }

  sealed trait Rank {
    def value: Int

    def >(that: Rank): Boolean = this.value > that.value
    def ==(that: Rank): Boolean = this.value == that.value
  }

  object Rank {

    def apply(rank: String): Rank = rank match {
      case "A" => Ace
      case "2" => Two
      case "3" => Three
      case "4" => Four
      case "5" => Five
      case "6" => Six
      case "7" => Seven
      case "8" => Eight
      case "9" => Nine
      case "t" => Ten
      case "J" => Jack
      case "Q" => Queen
      case "K" => King
    }

    case object Ace   extends Rank { override def value: Int = 14 }
    case object King  extends Rank { override def value: Int = 13 }
    case object Queen extends Rank { override def value: Int = 12 }
    case object Jack  extends Rank { override def value: Int = 11 }
    case object Ten   extends Rank { override def value: Int = 10 }
    case object Nine  extends Rank { override def value: Int = 9 }
    case object Eight extends Rank { override def value: Int = 8 }
    case object Seven extends Rank { override def value: Int = 7 }
    case object Six   extends Rank { override def value: Int = 6 }
    case object Five  extends Rank { override def value: Int = 5 }
    case object Four  extends Rank { override def value: Int = 4 }
    case object Three extends Rank { override def value: Int = 3 }
    case object Two   extends Rank { override def value: Int = 2 }
  }

  sealed trait Colors

  object Colors {

    def apply(colors: String): Colors = colors match {
      case "s" => Spades
      case "h" => Hearts
      case "d" => Diamonds
      case "c" => Clubs
    }

    case object Spades   extends Colors
    case object Hearts   extends Colors
    case object Diamonds extends Colors
    case object Clubs    extends Colors
  }

  final case class Card(value: String) {
    def rank: Rank = Rank(value.substring(0, 1))
    def colors: Colors = Colors(value.substring(1))
    def rankValue: Int = rank.value
  }

  trait HasValue {
    def value: String
  }

  sealed trait HasCards extends HasValue {
    def cards: List[Card] = value.sliding(2, 2).toList.map(Card.apply)
  }

  object HasCards {
    final case class Hand(value: String)                                                            extends HasCards
    final case class Player(value: String, handValue: Option[HandValue] = None, rankValue: Int = 0) extends HasCards

    object Player {
      def apply(value: String): Player = new Player(value)
    }
  }

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

    def isStraightFlush(hand: Hand, player: Player): Boolean = {
      hand.cards.combinations(3).toList.exists { cards =>
        val allCards = (cards ::: player.cards).sorted
        val allRankValues = allCards.map(_.rankValue)
        val color = allCards.head.colors

        straightRanges.contains(allRankValues) && allCards.forall(_.colors == color)
      }
    }

    def isFourOfAKind(hand: Hand, player: Player): Boolean = {
      val playerRanks = player.cards.map(_.rank)
      val (first, second) = (playerRanks.head, playerRanks.last)
      if (first == second) hand.cards.count(_.rank == first) == 2
      else hand.cards.count(_.rank == first) == 4 || hand.cards.count(_.rank == second) == 4
    }

    def isFullHouse(hand: Hand, player: Player): Boolean = isThreeOfAKind(hand, player) && isPair(hand, player)

    def isThreeOfAKind(hand: Hand, player: Player): Boolean = {
      val playerRanks = player.cards.map(_.rank)
      val (first, second) = (playerRanks.head, playerRanks.last)
      if (first == second) hand.cards.count(_.rank == first) == 1
      else hand.cards.count(_.rank == first) == 3 || hand.cards.count(_.rank == second) == 3
    }

    def isFlush(hand: Hand, player: Player): Boolean = {
      val playerColors = player.cards.map(_.colors)
      val (first, second) = (playerColors.head, playerColors.last)
      if (first == second) hand.cards.count(_.colors == first) >= 3
      else hand.cards.count(_.colors == first) >= 4 || hand.cards.count(_.colors == second) >= 4
    }

    def isPair(hand: Hand, player: Player): Boolean = {
      player.cards.head == player.cards.last || {
        val (first, second) = (player.cards.head, player.cards.last)

        hand.cards.count(_ == first) == 2 || hand.cards.count(_ == second) == 2
      }
    }

    def isTwoPairs(hand: Hand, player: Player): Boolean = {
      val (first, second) = (player.cards.head, player.cards.last)
      if (first == second) hand.cards.toSet.size <= 4
      else hand.cards.count(_ == first) == 1 && hand.cards.count(_ == second) == 1
    }

    def isStraight(hand: Hand, player: Player): Boolean = {
      val combinations = hand.cards.combinations(3).toList
      combinations.exists { cards =>
        val allCards = (cards ::: player.cards).sorted
        val allCardsValues = allCards.map(_.rank.value)

        straightRanges contains allCardsValues
      }
    }

    case object StraightFlush extends HandValue { override def value: Int = 9 }
    case object FourOfAKind   extends HandValue { override def value: Int = 8 }
    case object FullHouse     extends HandValue { override def value: Int = 7 }
    case object Flush         extends HandValue { override def value: Int = 6 }
    case object Straight      extends HandValue { override def value: Int = 5 }
    case object ThreeOfAKind  extends HandValue { override def value: Int = 4 }
    case object TwoPairs      extends HandValue { override def value: Int = 3 }
    case object Pair          extends HandValue { override def value: Int = 2 }
    case object HighCard      extends HandValue { override def value: Int = 1 }
  }

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
        if (straightRanges.contains(sortedRanks) && sorted.forall(c => c.colors == first.colors || c.colors == second.colors)) Some(sortedRanks.map(_.value).sum) else None
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
      val rankValue = if (first.colors == second.colors) filterByColorAndSumN(hand, first.colors, 3) + first.rankValue + second.rankValue
       else if (hand.cards.count(_.colors == first.colors) >= 4) filterByColorAndSumN(hand, first.colors, 4) + first.rankValue
       else filterByColorAndSumN(hand, second.colors, 4)+ second.rankValue

      player.copy(handValue = Some(Flush), rankValue = rankValue)
    }

    private def filterByColorAndSumN(hand: Hand, colors: Colors, n: Int): Int =
      hand.cards.filter(_.colors == colors).sorted.reverse.take(n).map(_.rankValue).sum

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
        val possibleCards = TreeMap.from(hand.cards.groupBy(_.rank).filter { case (_, value) =>  value.size > 1 })
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

}