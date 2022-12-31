package domain

import munit.FunSuite
import domain.HasCards._
import domain.HandValue._

class TableTest extends FunSuite {

  test("getHighCard = 24") {
    val player = Table.getHighCard(Player("Ac3d"))
    assertEquals(player.rankValue, HighCard.value + Rank.Ace.value)
  }

  test("getHighCard = 19") {
    val player = Table.getHighCard(Player("9d2s"))
    assertEquals(player.rankValue, HighCard.value + Rank.Nine.value)
  }

  test("getHighCard = 21") {
    val player = Table.getHighCard(Player("QdQs"))
    assertEquals(player.rankValue, HighCard.value + Rank.Queen.value)
  }

  test("getPair = 128") {
    val hand = Hand("KcQc3d6sTs")
    val player = Table.getPair(hand, Player("AdAc"))
    assertEquals(player.rankValue, Pair.value + Rank.Ace.value * 2)
  }

  test("getPair = 118") {
    val hand = Hand("9dAd2d3d4d")
    val player = Table.getPair(hand, Player("9cTc"))
    assertEquals(player.rankValue, Pair.value + Rank.Nine.value * 2)
  }

  test("full test") {
    val hand = Hand("Ac2d3s8sQs")
    val players = List(
      Player("Ad4s"),
      Player("2s2h")
    )
    val table = Table(hand, players)
    val solved = table.evaluateHand

    println()
  }


}
