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
    val isPair = HandValue.isPair(hand, player)
    assert(isPair)
  }

  test("isPair = true") {
    val hand = Hand("2c5dKcQh9d")
    val player = Player("TcTs")
    val isPair = HandValue.isPair(hand, player)
    assert(isPair)
  }

  test("isPair = true") {
    val hand = Hand("2c5dKcQhAc")
    val player = Player("3cAs")
    val isPair = HandValue.isPair(hand, player)
    assert(isPair)
  }

  test("isPair = false") {
    val hand = Hand("2c5dKcQh9d")
    val player = Player("6c7s")
    val isPair = HandValue.isPair(hand, player)
    assert(!isPair)
  }

  test("isPair = false") {
    val hand = Hand("2c5dKcQhQd")
    val player = Player("Ac8s")
    val isPair = HandValue.isPair(hand, player)
    assert(!isPair)
  }

  test("isTwoPairs = true") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("3c4h")
    val isPair = HandValue.isTwoPairs(hand, player)
    assert(isPair)
  }

  test("isTwoPairs = true") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("2d3h")
    val isPair = HandValue.isTwoPairs(hand, player)
    assert(isPair)
  }

  test("isTwoPairs = false") {
    val hand = Hand("2c3dKcQh4d")
    val player = Player("3c3h")
    val isPair = HandValue.isTwoPairs(hand, player)
    assert(!isPair)
  }


}
