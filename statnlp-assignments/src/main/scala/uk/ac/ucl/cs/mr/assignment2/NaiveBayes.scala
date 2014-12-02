package uk.ac.ucl.cs.mr.assignment2

import ml.wolfe.nlp.Token
import uk.ac.ucl.cs.mr.assignment2.LoadData.FeatureTemplate
import scala.collection.mutable.{ListBuffer, HashMap}
import cc.factorie.la.{GrowableSparseTensor1 => SparseVector}

/**
 * Created by fayimora on 30/11/14.
 */
class NaiveBayes(labelTokens: HashMap[String, ListBuffer[Token]],
                 featureTemplates: Seq[FeatureTemplate]) {

  val candidate = if(labelTokens.keys.size > 3) "trigger" else "argument"
  val vocabulary = labelTokens.values.flatten.toSeq

  def prob(token: Token, trueLabels: Seq[String]): (String, Double) = {
    val pLabels = trueLabels map probLabel
    val pWordLabels = trueLabels.map(l => probTokenLabel(token, l))
    val pFeatLabels = trueLabels map probFeatLabel

    val probabilities: Seq[Double] = for(i <- 0 to trueLabels.size-1) yield pLabels(i) * pWordLabels(i) * pFeatLabels(i)
    val idx = probabilities.toSeq.zipWithIndex.maxBy(_._1)._2
    (trueLabels(idx), probabilities(idx))
  }

  /**
   * This function calculates the probability of a given label.
   *
   * @param label is the label of either the triggers or arguments. It could be Regulation, None, Theme, e.t.c
   * @return the probability of the argument label
   */
  def probLabel(label: String): Double =
    labelTokens(label).size / labelTokens.values.map(_.size).sum.toDouble

  /**
   * This function calculates the probability of a token given a label
   *
   * @param token the token whose probability we are interested in
   * @param label the label whose prior we want to use
   * @return the probability that of a word given a label
   */
  def probTokenLabel(token: Token, label: String): Double =
    labelTokens(label).count(_.word == token.word)+1 / labelTokens(label).size.toDouble

  /**
   * This function returns the probability of features for a given label
   * @param label label used to calculate the probability
   * @return probability of features given a label
   */
  def probFeatLabel(label: String): Double = {
    val tokens = labelTokens(label)
    var prod = 1.0
    for(ft <- featureTemplates) {
      val tempVec = new SparseVector(Nil)
      for((token,i) <- tokens.zipWithIndex) {
        tempVec += (i, ft(token, label, candidate))
      }
      val p = tempVec.sum/tokens.size
      prod = prod * p
    }
    prod
  }
}
