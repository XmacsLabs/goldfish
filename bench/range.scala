@main def calculateSumOfSquares(): Unit =
  val startTime = System.nanoTime()

  val results = (1 to 10000).map: _ =>
    (1L to 10000L).filter(x => x % 3 == 0)

  val endTime = System.nanoTime()
  val duration = (endTime - startTime) / 1e6d

  println(s"Result (first iteration): ${results.head.length}")  // 打印第一个结果
  println(s"Total time for 1000 iterations: $duration msec")
