package domain

import munit.FunSuite
import HasCards._

// c = clubs
// d = diamonds
// h = hearts
// s = spades
class HandValueTest extends FunSuite {

  test("isPair = true") {
    val hand = Hand("2c3dKcQh9d")
    val player = Player("3c4h")
    assert(HandValue.isPair(hand, player))
  }

  test("isPair = true") {
    val hand = Hand("2c5dKcQh9d")
    val player = Player("TcTs")
    assert(HandValue.isPair(hand, player))
  }

  test("isPair = true") {
    val hand = Hand("2c5dKcQhAc")
    val player = Player("3cAs")
    assert(HandValue.isPair(hand, player))
  }

  test("isPair = false") {
    val hand = Hand("2c5dKcQh9d")
    val player = Player("6c7s")
    assert(!HandValue.isPair(hand, player))
  }

  test("isPair = false") {
    val hand = Hand("2c5dKcQhQd")
    val player = Player("Ac8s")
    assert(!HandValue.isPair(hand, player))
  }

  test("isTwoPairs = true") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("3c4h")
    assert(HandValue.isTwoPairs(hand, player))
  }

  test("isTwoPairs = true") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("2d3h")
    assert(HandValue.isTwoPairs(hand, player))
  }

  test("isTwoPairs = false") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("3c3h")
    assert(!HandValue.isTwoPairs(hand, player))
  }

  test("isTwoPairs = false") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("3c5h")
    assert(!HandValue.isTwoPairs(hand, player))
  }

  test("isTwoPairs = false") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("6c2h")
    assert(!HandValue.isTwoPairs(hand, player))
  }

  test("isThreeOfAKind = true") {
    val hand = Hand("2c3dKc6h4d")
    val player = Player("6c6h")
    assert(HandValue.isThreeOfAKind(hand, player))
  }

  test("isThreeOfAKind = true") {
    val hand = Hand("2c6dKc6h4d")
    val player = Player("6c7h")
    assert(HandValue.isThreeOfAKind(hand, player))
  }

  test("isThreeOfAKind = true") {
    val hand = Hand("2c6d7c7d4d")
    val player = Player("6c7h")
    assert(HandValue.isThreeOfAKind(hand, player))
  }

  test("isThreeOfAKind = false") {
    val hand = Hand("2c5d7c7d4d")
    val player = Player("TcTh")
    assert(!HandValue.isThreeOfAKind(hand, player))
  }

  test("isThreeOfAKind = false") {
    val hand = Hand("2c6d7c7d4d")
    val player = Player("AcTh")
    assert(!HandValue.isThreeOfAKind(hand, player))
  }

  test("isStraight = true") {

  }

}
