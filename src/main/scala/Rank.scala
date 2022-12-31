
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

