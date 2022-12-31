package domain

import munit.FunSuite
import domain.HasCards._
import domain.HandValue._

class TableTest extends FunSuite {

  test("getHighCard = 114") {
    val player = Table.getHighCard(Player("Ac3d"))
    assertEquals(player.rankValue, HighCard.value + Rank.Ace.value)
  }

  test("getHighCard = 109") {
    val player = Table.getHighCard(Player("9d2s"))
    assertEquals(player.rankValue, HighCard.value + Rank.Nine.value)
  }

  test("getHighCard = 112") {
    val player = Table.getHighCard(Player("QdQs"))
    assertEquals(player.rankValue, HighCard.value + Rank.Queen.value)
  }

  test("getPair = 228") {
    val hand = Hand("KcQc3d6sTs")
    val player = Table.getPair(hand, Player("AdAc"))
    assertEquals(player.rankValue, Pair.value + Rank.Ace.value * 2)
  }

  test("getPair = 218") {
    val hand = Hand("9dAd2d3d4d")
    val player = Table.getPair(hand, Player("9cTc"))
    assertEquals(player.rankValue, Pair.value + Rank.Nine.value * 2)
  }

  test("getPair = 220") {
    val hand = Hand("TcAcAs2s4h")
    val player = Table.getPair(hand, Player("Ts3s"))
    assertEquals(player.rankValue, Pair.value + Rank.Ten.value * 2)
  }

  test("getTwoPairs = 330") {
    val hand = Hand("4cTc2d3d5h")
    val player = Table.getTwoPairs(hand, Player("5sTs"))
    assertEquals(player.rankValue, TwoPairs.value + Rank.Ten.value * 2 + Rank.Five.value * 2)
  }

  test("getThreeOfAKind = 442") {
    val hand = Hand("As3d4cTcTs")
    val player = Table.getThreeOfAKind(hand, Player("AcAh"))
    assertEquals(player.rankValue, ThreeOfAKind.value + Rank.Ace.value * 3)
  }

  test("getThreeOfAKind = 430") {
    val hand = Hand("Qs3d4cTcTs")
    val player = Table.getThreeOfAKind(hand, Player("AcTh"))
    assertEquals(player.rankValue, ThreeOfAKind.value + Rank.Ten.value * 3)
  }

  test("getThreeOfAKind = 406") {
    val hand = Hand("Qs3d4c2c2h")
    val player = Table.getThreeOfAKind(hand, Player("2sTh"))
    assertEquals(player.rankValue, ThreeOfAKind.value + Rank.Two.value * 3)
  }

  test("getStraight = 525") {
    val hand = Hand("2d3c4s6hKc")
    val player = Table.getStraight(hand, Player("5s7d"))
    assertEquals(player.rankValue, Straight.value + Rank.Three.value + Rank.Four.value + Rank.Five.value + Rank.Six.value + Rank.Seven.value)
  }

  test("getStraight = 525") {
    val hand = Hand("2dAcJsQhKc")
    val player = Table.getStraight(hand, Player("5sTd"))
    assertEquals(player.rankValue, Straight.value + Rank.Ten.value + Rank.Jack.value + Rank.Queen.value + Rank.King.value + Rank.Ace.value)
  }



}
