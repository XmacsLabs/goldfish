@main def calculateSumOfSquares(): Unit =
  val startTime = System.nanoTime()

  // 计算 1 到 10000 的平方和，重复 1000 次
  val results = (1 to 1000).map: _ =>
    (1L to 10000L).map(x => x * x).sum

  val endTime = System.nanoTime()
  val duration = (endTime - startTime) / 1e6d

  println(s"Result (first iteration): ${results.head}")  // 打印第一个结果
  println(s"Total time for 1000 iterations: $duration msec")
