package uk.ac.ucl.cs.mr.assignment2

/**
 * Created by fayimora on 01/12/14.
 */
class Evaluator(predictedLabels: Seq[String], trueLabels: Seq[String]) {
  lazy val zippedLabels = predictedLabels.zip(trueLabels)

  def precision(label: String): Double = {
    var (correctPredsCount, predCounts) = (0, 0)
    for((pred, gold) <- zippedLabels) {
      if (pred == label)
        if (pred == gold) correctPredsCount += 1
      else predCounts += 1
    }
    correctPredsCount/predCounts.toDouble
  }

  def recall(label: String): Double = {
    var (correctPredsCount, goldCounts) = (0, 0)
    for((pred, gold) <- zippedLabels) {
      if(pred == gold && pred != label) correctPredsCount += 1
      if(gold != label) goldCounts += 1
    }
    correctPredsCount/goldCounts.toDouble
  }

  def f1Measure(label: String): Double =
    2 * ( (precision(label) * recall(label)) / (precision(label) + recall(label)) )
}
