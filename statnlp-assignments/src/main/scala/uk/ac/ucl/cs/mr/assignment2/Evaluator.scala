package uk.ac.ucl.cs.mr.assignment2

/**
 * Created by fayimora on 01/12/14.
 */
class Evaluator(predictedLabels: Seq[String], trueLabels: Seq[String]) {
  lazy val zippedLabels = predictedLabels.zip(trueLabels)

  def precision(label: String): Double = {
    var (correctPredsCount, predCounts) = (0, 0)
    for((pred, gold) <- zippedLabels) {
      if(pred != label) {
        if (pred == gold) correctPredsCount += 1
        predCounts += 1
      }
    }
    (correctPredsCount/predCounts.toDouble) * 100
  }

  def recall(label: String): Double = {
    var (correctPredsCount, goldCounts) = (0, 0)
    for((pred, gold) <- zippedLabels) {
      if(pred == gold && pred != label) correctPredsCount += 1
      if(gold != label) goldCounts += 1
    }
    (correctPredsCount/goldCounts.toDouble) * 100// precision(label)
  }

  def f1Measure(label: String): Double =
    2 * ( (precision(label) * recall(label)) / (precision(label) + recall(label)) )

  def accuracy(): Double =
    (zippedLabels.count(t => t._1 == t._2) / zippedLabels.size.toDouble) * 100

  def accuracy(label: String): Double = {
    var nLabels = 0
    var nCorrectLabels = 0
    for((pred, gold) <- zippedLabels) {
      if(pred != label && pred==gold) nCorrectLabels += 1
      if(gold != label) nLabels += 1
//      nLabels += 1
    }
    (nCorrectLabels/nLabels.toDouble) * 100
  }
}
