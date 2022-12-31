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
    val hand = Hand("2c3d4ThQd")
    val player = Player("5d6d")
    assert(HandValue.isStraight(hand, player))
  }

  test("isStraight = true") {
    val hand = Hand("TcJdQhThQd")
    val player = Player("AcKs")
    assert(HandValue.isStraight(hand, player))
  }

  test("isStraight = true") {
    val hand = Hand("JsQh9s6h2d")
    val player = Player("TcKc")
    assert(HandValue.isStraight(hand, player))
  }

  test("isStraight = false") {
    val hand = Hand("JsQh9s6h2d")
    val player = Player("Tc2c")
    assert(!HandValue.isStraight(hand, player))
  }

  test("isStraight = false") {
    val hand = Hand("2c3d4s7hJc")
    val player = Player("5cTs")
    assert(!HandValue.isStraight(hand, player))
  }

  test("isFlush = true") {
    val hand = Hand("2c3c4c5d3d")
    val player = Player("TcAc")
    assert(HandValue.isFlush(hand, player))
  }

  test("isFlush = true") {
    val hand = Hand("2c3c4c5c3d")
    val player = Player("TcAd")
    assert(HandValue.isFlush(hand, player))
  }

  test("isFlush = true") {
    val hand = Hand("2d3d4d5c3d")
    val player = Player("TcAd")
    assert(HandValue.isFlush(hand, player))
  }

  test("isFlush = false") {
    val hand = Hand("2d3d4d5c3s")
    val player = Player("TdAs")
    assert(!HandValue.isFlush(hand, player))
  }

  test("isFlush = false") {
    val hand = Hand("2c3c4d5s3d")
    val player = Player("TcAc")
    assert(!HandValue.isFlush(hand, player))
  }

  test("isFlush = false") {
    val hand = Hand("2h3c4h5h3d")
    val player = Player("TcAh")
    assert(!HandValue.isFlush(hand, player))
  }

  test("isFullHouse = true") {
    val hand = Hand("ThTs4h5hQd")
    val player = Player("TcQh")
    assert(HandValue.isFullHouse(hand, player))
  }

  test("isFullHouse = true") {
    val hand = Hand("ThTs4h5hTd")
    val player = Player("AcAh")
    assert(HandValue.isFullHouse(hand, player))
  }

  test("isFullHouse = true") {
    val hand = Hand("Th5s4h5hTd")
    val player = Player("Tc5d")
    assert(HandValue.isFullHouse(hand, player))
  }

  test("isFullHouse = false") {
    val hand = Hand("Th6s4h5hQd")
    val player = Player("Tc5d")
    assert(!HandValue.isFullHouse(hand, player))
  }

  test("isFullHouse = false") {
    val hand = Hand("ThTs4h5hQd")
    val player = Player("AcAd")
    assert(!HandValue.isFullHouse(hand, player))
  }

  test("isFourOfAKind = true") {
    val hand = Hand("AsAh4h5hQd")
    val player = Player("AcAd")
    assert(HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = true") {
    val hand = Hand("AsAh4h5hAd")
    val player = Player("AcKd")
    assert(HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = true") {
    val hand = Hand("KsKh4h5hKc")
    val player = Player("AcKd")
    assert(HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = false") {
    val hand = Hand("AsKh4h5hKc")
    val player = Player("AcAd")
    assert(!HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = false") {
    val hand = Hand("QsKh4h5hKc")
    val player = Player("AcAd")
    assert(!HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = false") {
    val hand = Hand("QsKh4h5hQc")
    val player = Player("AcQd")
    assert(!HandValue.isFourOfAKind(hand, player))
  }

  test("isFourOfAKind = false") {
    val hand = Hand("QsKh4h5hKc")
    val player = Player("AcQd")
    assert(!HandValue.isFourOfAKind(hand, player))
  }

  test("isStraightFlush = true") {
    val hand = Hand("2c3c4c2d3d")
    val player = Player("5c6c")
    assert(HandValue.isStraightFlush(hand, player))
  }

  test("isStraightFlush = true") {
    val hand = Hand("QcKc9c2d3d")
    val player = Player("JcTc")
    assert(HandValue.isStraightFlush(hand, player))
  }

  test("isStraightFlush = true") {
    val hand = Hand("TcJc9c9s3d")
    val player = Player("QcKc")
    assert(HandValue.isStraightFlush(hand, player))
  }

  test("isStraightFlush = false") {
    val hand = Hand("Tc2c9c9s3d")
    val player = Player("QcKc")
    assert(!HandValue.isStraightFlush(hand, player))
  }

  test("isStraightFlush = false") {
    val hand = Hand("TcJc9c9s3d")
    val player = Player("QdKc")
    assert(!HandValue.isStraightFlush(hand, player))
  }

  test("isStraightFlush = false") {
    val hand = Hand("TcJs9c9s3d")
    val player = Player("QcKc")
    assert(!HandValue.isStraightFlush(hand, player))
  }


}
