package com.example.gildedRose

import org.scalatest.{FlatSpec, Matchers}

class GildedRoseSpec extends FlatSpec with Matchers {

  import GildedRose.updateItems

  "updateItems" should "should decrease normal items quality" in {
    updateItems(Seq(
      StandardItem("Book", 15, 10)
    )) shouldEqual Seq(
      StandardItem("Book", 14, 9)
    )
  }

  "updateItems" should "should decrease normal items quality at twice the speed when sellIn is 0" in {
    updateItems(Seq(
      StandardItem("Book", 15, 0)
    )) shouldEqual Seq(
      StandardItem("Book", 13, 0)
    )
  }

  "updateItems" should "should never decrease quality bellow 0" in {
    updateItems(Seq(
      StandardItem("Book", 0, 1)
    )) shouldEqual Seq(
      StandardItem("Book", 0, 0)
    )
  }

  "updateItems" should "should increase aged items quality" in {
    updateItems(Seq(
      AgeingItem("Aged Brie", 0, 1)
    )) shouldEqual Seq(
      AgeingItem("Aged Brie", 1, 0)
    )
  }

  "updateItems" should "should never increase aged items quality above 50" in {
    updateItems(Seq(
      AgeingItem("Aged Brie", 50, 1)
    )) shouldEqual Seq(
      AgeingItem("Aged Brie", 50, 0)
    )
  }

  "updateItems" should "should never decrease Conjured items quality bellow 0" in {
    updateItems(Seq(
      ConjuredItem("Conjured", 0, 1)
    )) shouldEqual Seq(
      ConjuredItem("Conjured", 0, 0)
    )
  }

  "updateItems" should "should decrease Conjured items quality bellow twice the rate of the normal one when normal" in {
    updateItems(Seq(
      ConjuredItem("Conjured", 10, 1)
    )) shouldEqual Seq(
      ConjuredItem("Conjured", 8, 0)
    )
  }

  "updateItems" should "should decrease Conjured items quality bellow twice the rate of the normal one when past sell in" in {
    updateItems(Seq(
      ConjuredItem("Conjured", 10, 0)
    )) shouldEqual Seq(
      ConjuredItem("Conjured", 6, 0)
    )
  }

  "updateItems" should "should not impact Back Stage Pass items quality when sellIn above or equals to 10" in {
    updateItems(Seq(
      BackStagePass("Back Stage Pass", 10, 10)
    )) shouldEqual Seq(
      BackStagePass("Back Stage Pass", 10, 9)
    )
  }

  "updateItems" should "should increase Back Stage Pass items quality by 2 when sellIn between 10 and 5" in {
    updateItems(Seq(
      BackStagePass("Back Stage Pass", 10,9)
    )) shouldEqual Seq(
      BackStagePass("Back Stage Pass", 12, 8)
    )
  }

  "updateItems" should "should increase Back Stage Pass items quality by 3 when sellIn between 5 and 0" in {
    updateItems(Seq(
      BackStagePass("Back Stage Pass", 12, 4)
    )) shouldEqual Seq(
      BackStagePass("Back Stage Pass", 15, 3)
    )
  }

  "updateItems" should "should decrease Back Stage Pass items quality to 0 past sell in" in {
    updateItems(Seq(
      BackStagePass("Back Stage Pass", 15, 0)
    )) shouldEqual Seq(
      BackStagePass("Back Stage Pass", 0, 0)
    )
  }
}
