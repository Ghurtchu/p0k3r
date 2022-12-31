
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
