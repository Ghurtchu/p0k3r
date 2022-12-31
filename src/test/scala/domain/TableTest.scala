package domain

import munit.FunSuite
import domain.HasCards._

class TableTest extends FunSuite {

  test("getHighCard = 14") {
    val player = Table.getHighCard(Player("Ac3d"))
    assertEquals(player.rankValue, Rank.Ace.value)
  }

  test("getHighCard = 9") {
    val player = Table.getHighCard(Player("9d2s"))
    assertEquals(player.rankValue, Rank.Nine.value)
  }

  test("getHighCard = 11") {
    val player = Table.getHighCard(Player("QdQs"))
    assertEquals(player.rankValue, Rank.Queen.value)
  }

}
