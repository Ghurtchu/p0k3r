package domain

sealed trait Color

object Color {

  def apply(colors: String): Color = colors match {
    case "s" => Spades
    case "h" => Hearts
    case "d" => Diamonds
    case "c" => Clubs
  }

  case object Spades   extends Color
  case object Hearts   extends Color
  case object Diamonds extends Color
  case object Clubs    extends Color
}