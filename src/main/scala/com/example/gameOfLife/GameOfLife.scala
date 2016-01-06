package com.example.gameOfLife

import scala.util.Random

object GameOfLife {
  def buildWorld(x: Int, y: Int, seed: Long): World = {
    val random = new Random(seed)
    def isAlive() = random.nextBoolean()
    val result = World(Array.tabulate(x, y) { case (xIdx, yIdx) => Cell(xIdx, yIdx, isAlive = isAlive()) })
    result
  }

  def tick(world: World): World = {
    import CellLifecycle.shouldBeAliveOnNextTick
    val cells: Array[Array[Cell]] = Array.ofDim(world.width, world.height)
    (0 to world.width - 1).foreach { x =>
      Array.copy(world.cells(x), 0, cells(x), 0, world.cells(x).length)
    }

    val newWorld = World(cells)

    (0 to newWorld.width - 1).foreach { x =>
      (0 to newWorld.height - 1).foreach { y =>
        val currentCell = world.cells(x)(y)
        newWorld.cells(x).update(y, currentCell.copy(isAlive = shouldBeAliveOnNextTick(currentCell, world)))
      }
    }

    newWorld
  }

  def play(world: World, ticks: Int): World = {
    (0 to ticks - 1).foldLeft(world) { case (world, t) => tick(world)}
  }
}

object CellLifecycle {
  def shouldBeAliveOnNextTick(cell: Cell, world: World): Boolean = {
    shouldLive(cell, world) || shouldReproduce(cell, world)
  }

  def isOverpopulated(cell: Cell, world: World): Boolean = {
    neighbors(cell, world) > 3
  }

  def isUnderpopulated(cell: Cell, world: World): Boolean = {
    neighbors(cell, world) < 2
  }

  def shouldLive(cell: Cell, world: World): Boolean = {
    !isOverpopulated(cell, world) && !isUnderpopulated(cell, world) && cell.isAlive
  }

  def shouldReproduce(cell: Cell, world: World): Boolean = {
    neighbors(cell, world) == 3 && !cell.isAlive
  }

  def neighbors(cell: Cell, world: World): Int = {
    def offsetWithBoundary(index: Int, offsetToTry: Int, max: Int) = {
      val proposedIndex = index + offsetToTry
      if (proposedIndex < 0 || proposedIndex >= max) index else proposedIndex
    }

    def cellToInt(x: Int, y: Int) = if (world.cells(x)(y).isAlive) 1 else 0
    val x1 = offsetWithBoundary(cell.x, - 1, world.width)
    val y1 = offsetWithBoundary(cell.y, - 1, world.height)
    val x2 = offsetWithBoundary(cell.x, 1, world.width)
    val y2 = offsetWithBoundary(cell.y, 1, world.height)

    (x1 to x2).map { x =>
      (y1 to y2).map { y =>
        cellToInt(x, y)
      }.reduce(_ + _)
    }.reduce(_ + _) - cellToInt(cell.x, cell.y)
  }
}

case class Cell(x: Int, y: Int, isAlive: Boolean)

case class World(cells: Array[Array[Cell]]) {
  lazy val width = cells.length
  lazy val height = if (cells.length > 0) cells(1).length else 0

  override def hashCode(): Int = {
    var hash = 31;
    (0 to cells.length).foreach { x =>
      (0 to cells(x).length).foreach { y =>
        hash = hash + cells(x)(y).hashCode()
      }
    }

    hash
  }

  override def equals(obj: scala.Any): Boolean = obj match {
    case w: World => {
      var equals = w.width == width && w.height == height
      if (equals) {
        (0 to cells.length - 1).foreach { x =>
          (0 to cells(x).length - 1).foreach { y =>
            equals = equals && cells(x)(y).equals(w.cells(x)(y))
          }
        }
      }
      equals
    }
    case _ => false
  }
}
