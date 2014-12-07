package uk.ac.ucl.cs.mr.assignment2

import cc.factorie.la.{GrowableSparseIndexedTensor1 => SparseVector}
import ml.wolfe.nlp.Token
import uk.ac.ucl.cs.mr.assignment2.LoadData.FeatureTemplate

/**
 * Created by fayimora on 02/12/14.
 */
class LogLinearModel(weights: SparseVector,
                     featureTemplates: Seq[FeatureTemplate]) extends Model {

  def prob(token: Token, label: String, candidate: String): Double = {
    val featVec = new SparseVector(Nil)
    for((ft, i) <- featureTemplates.zipWithIndex) featVec += (i, ft(token, label, candidate))
    math.exp(featVec dot weights)
  }

  /**
   * This function is used during prediction. It returns a tuple of (predictedLabel, likelihood)
   * @param token
   * @param trueLabels
   * @return
   */
  def prob(token: Token, trueLabels: Seq[String]): (String, Double) = {
    val candidate = if(trueLabels.size > 3) "trigger" else "argument"

    val probabilities: Seq[Double] = trueLabels.map(l => prob(token, l, candidate))
    val idx = probabilities.zipWithIndex.maxBy(_._1)._2
    (trueLabels(idx), probabilities(idx))
  }
}
