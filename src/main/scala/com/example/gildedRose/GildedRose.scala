package com.example.gildedRose

sealed trait Item {
  def name: String
  def quality: Int
  def sellIn: Option[Int]
}

case object Sulfur extends Item {
  val name: String = "Sulfur"
  val quality: Int = 80
  val sellIn: Option[Int] = Option.empty
}

case class StandardItem(name: String, quality: Int, expiresIn: Int) extends Item {
  val sellIn = Option(expiresIn)
}

case class ConjuredItem(name: String, quality: Int, expiresIn: Int) extends Item {
  val sellIn = Option(expiresIn)
}

case class AgeingItem(name: String, quality: Int, expiresIn: Int) extends Item {
  val sellIn = Option(expiresIn)
}

case class BackStagePass(name: String, quality: Int, expiresIn: Int) extends Item {
  val sellIn = Option(expiresIn)
}

object GildedRose {
  type Items = Seq[Item]
  def updateItems(items: Items): Items = {
    items.map(QualityReductionStrategy.reduce)
  }
}

object QualityReductionStrategy {
  def reduce(item: Item) = {
    val expiresIn = item.sellIn.map(v => if (v > 0) v - 1 else 0).getOrElse(0)
    item match {
      case Sulfur => Sulfur
      case i: StandardItem if i.expiresIn > 0 => i.copy(quality = standardQualityReduction(i.quality), expiresIn = expiresIn)
      case i: StandardItem => i.copy(quality = expiredQualityReduction(i.quality), expiresIn = expiresIn)
      case i: ConjuredItem if i.expiresIn > 0 => i.copy(quality = conjuredQualityReduction(i.quality), expiresIn = expiresIn)
      case i: ConjuredItem => i.copy(quality = expiredConjuredQualityReduction(i.quality), expiresIn = expiresIn)
      case i: AgeingItem => i.copy(quality = agedQualityReduction(i.quality), expiresIn = expiresIn)
      case i: BackStagePass if i.expiresIn >= 10 =>
        i.copy(quality = moreThan10DaysBackstagePackStagePassQualityReduction(i.quality), expiresIn = expiresIn)
      case i: BackStagePass if i.expiresIn < 10 && i.expiresIn > 5 =>
        i.copy(quality = between10and5DaysBackstagePackStagePassQualityReduction(i.quality), expiresIn = expiresIn)
      case i: BackStagePass if i.expiresIn < 5 && i.expiresIn > 0 =>
        i.copy(quality = between5and0DaysBackstagePackStagePassQualityReduction(i.quality), expiresIn = expiresIn)
      case i: BackStagePass if i.expiresIn == 0 =>
        i.copy(quality = expiredBackstagePackStagePassQualityReduction(i.quality), expiresIn = expiresIn)
    }
  }

  def standardQualityReduction(quality: Int) = normalize(quality - 1)
  def expiredQualityReduction(quality: Int) = normalize(quality - 2)
  def conjuredQualityReduction(quality: Int) = normalize(quality - 2)
  def expiredConjuredQualityReduction(quality: Int) = normalize(quality - 4)
  def agedQualityReduction(quality: Int) = normalize(quality + 1)
  def moreThan10DaysBackstagePackStagePassQualityReduction(quality: Int) = normalize(quality)
  def between10and5DaysBackstagePackStagePassQualityReduction(quality: Int) = normalize(quality + 2)
  def between5and0DaysBackstagePackStagePassQualityReduction(quality: Int) = normalize(quality + 3)
  def expiredBackstagePackStagePassQualityReduction(quality: Int) = 0

  def normalize(quality: Int) = if (quality > 50) 50 else if (quality < 0) 0 else quality
}