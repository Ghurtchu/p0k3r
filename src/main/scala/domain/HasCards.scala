package domain

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