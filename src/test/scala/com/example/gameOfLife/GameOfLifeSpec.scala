package com.example.gameOfLife

import org.scalatest.{Matchers, FlatSpec}

class GameOfLifeSpec extends FlatSpec with Matchers {

  import GameOfLife._

  val mockWorld = World(Array(
    Array(Cell(0, 0, isAlive = true), Cell(0, 1, isAlive = false), Cell(0, 2, isAlive = false), Cell(0, 3, isAlive = false), Cell(0, 4, isAlive = false)),
    Array(Cell(1, 0, isAlive = true), Cell(1, 1, isAlive = false), Cell(1, 2, isAlive = false), Cell(1, 3, isAlive = true), Cell(1, 4, isAlive = false)),
    Array(Cell(2, 0, isAlive = false), Cell(2, 1, isAlive = true), Cell(2, 2, isAlive = true), Cell(2, 3, isAlive = true), Cell(2, 4, isAlive = true)),
    Array(Cell(3, 0, isAlive = false), Cell(3, 1, isAlive = false), Cell(3, 2, isAlive = false), Cell(3, 3, isAlive = true), Cell(3, 4, isAlive = false)),
    Array(Cell(4, 0, isAlive = false), Cell(4, 1, isAlive = true), Cell(4, 2, isAlive = false), Cell(4, 3, isAlive = false), Cell(4, 4, isAlive = false))
  ))

  "buildWorld" should "create new world correctly" in {
    val world = buildWorld(5, 5, 10L)
    world shouldBe mockWorld
  }

  "run" should "run the game correctly for 1 turn" in {
    val world = play(mockWorld, 1)
    world shouldBe World(Array(
      Array(Cell(0, 0, isAlive = false), Cell(0, 1, isAlive = false), Cell(0, 2, isAlive = false), Cell(0, 3, isAlive = false), Cell(0, 4, isAlive = false)),
      Array(Cell(1, 0, isAlive = true), Cell(1, 1, isAlive = false), Cell(1, 2, isAlive = false), Cell(1, 3, isAlive = true), Cell(1, 4, isAlive = true)),
      Array(Cell(2, 0, isAlive = false), Cell(2, 1, isAlive = true), Cell(2, 2, isAlive = false), Cell(2, 3, isAlive = false), Cell(2, 4, isAlive = true)),
      Array(Cell(3, 0, isAlive = false), Cell(3, 1, isAlive = true), Cell(3, 2, isAlive = false), Cell(3, 3, isAlive = true), Cell(3, 4, isAlive = true)),
      Array(Cell(4, 0, isAlive = false), Cell(4, 1, isAlive = false), Cell(4, 2, isAlive = false), Cell(4, 3, isAlive = false), Cell(4, 4, isAlive = false))
    ))
  }

  import CellLifecycle._

  "neighbors" should "return the correct count of alive neighbors when there is one in the corner" in {
    neighbors(Cell(0, 0, isAlive = true), mockWorld) shouldEqual 1
  }

  "neighbors" should "return the correct count of alive neighbors when there are 3 in the center" in {
    neighbors(Cell(3, 1, isAlive = false), mockWorld) shouldEqual 3
  }

  "isOverpopulated" should "return if cell is overpopulated" in {
    isOverpopulated(Cell(3, 2, isAlive = false), mockWorld) shouldEqual true
    isOverpopulated(Cell(2, 0, isAlive = false), mockWorld) shouldEqual false
  }

  "isUnderpopulated" should "return if cell is underpopulated" in {
    isUnderpopulated(Cell(4, 4, isAlive = false), mockWorld) shouldEqual true
    isUnderpopulated(Cell(3, 2, isAlive = false), mockWorld) shouldEqual false
  }

  "shouldLive" should "return if cell should live" in {
    shouldLive(Cell(2, 1, isAlive = true), mockWorld) shouldEqual true
    shouldLive(Cell(1, 1, isAlive = false), mockWorld) shouldEqual false
    shouldLive(Cell(4, 1, isAlive = false), mockWorld) shouldEqual false
  }

  "shouldReproduce" should "return if cell should reproduce" in {
    shouldReproduce(Cell(3, 1, isAlive = false), mockWorld) shouldEqual true
    shouldReproduce(Cell(2, 2, isAlive = true), mockWorld) shouldEqual false
    shouldReproduce(Cell(2, 0, isAlive = false), mockWorld) shouldEqual false
  }
}
