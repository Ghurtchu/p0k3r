import domain.HasCards._
import domain.{Game, Table}

object Main {
  def main(args: Array[String]): Unit = {
    val game = Game(args(0))
    game match {
      case Some(_) =>
        val hand = Hand(args(1))
        val players = args.slice(2, args.length).map(Player.apply).toList
        val table = Table(hand, players)
        val playersSorted = table.evaluateHand

        println(s"Hand: ${hand.cards.map(_.value).mkString("[",", ", "]")}")

        playersSorted.zipWithIndex.foreach { case (tuple, i) =>
          println(s"Place: ${i + 1} ~> ${tuple._2.mkString("[", ", ", "]")}")
        }

      case None =>
    }
  }

}