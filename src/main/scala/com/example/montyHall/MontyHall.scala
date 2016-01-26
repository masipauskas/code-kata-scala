package com.example.montyHall

object MontyHall {
  type Random = (Int) => Int
  def play(numberOfDoors: Int = 3)(random: Random = random)(strategy: (IndexedSeq[Result], Random) => Result): Result = {
    require(numberOfDoors >= 3)
    val generate = generateDoor(_: Int)(random)
    val game = strategy(_: IndexedSeq[Result], random)
    val doors = generate(numberOfDoors)
    game(doors)
  }

  def playGameVariableChange(doors: IndexedSeq[Result], random: Random) = {
    val pick = pickDoor(_: IndexedSeq[Result])(random)
    val (_, remainingDoors) = pick(doors)
    var pickResult: (Result, IndexedSeq[Result]) = (Lose, IndexedSeq())
    do {
      pickResult = pick(remainingDoors)
    } while (pickResult._1 == Win)

    val (result, _) = pick(pickResult._2)
    result
  }

  def playGameFirstChoice(doors: IndexedSeq[Result], random: Random) = {
    val (result, _) = pickDoor(doors)(random)
    result
  }

  def generateDoor(numberOfDoors: Int)(random: Random): IndexedSeq[Result] = {
    val winningDoor = random(numberOfDoors)
    (0 to numberOfDoors).map { door =>
      if (door == winningDoor) Win else Lose
    }
  }

  def pickDoor(remainingDoors: IndexedSeq[Result])(random: Random): (Result, IndexedSeq[Result]) = {
    val door = random(remainingDoors.length)
     (remainingDoors(door),
      remainingDoors.take(door) ++ (if (remainingDoors.length > door) remainingDoors.slice(door + 1, remainingDoors.length)
                                    else IndexedSeq()))
  }


  def random(max: Int): Int = {
    val random = new java.util.Random()
    random.nextInt(max)
  }

  sealed trait Result
  object Win extends Result
  object Lose extends Result
}
