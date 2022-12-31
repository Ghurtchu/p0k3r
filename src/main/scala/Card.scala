
final case class Card(value: String) {
  def rank: Rank = Rank(value.substring(0, 1))
  def color: Color = Color(value.substring(1))
  def rankValue: Int = rank.value
}