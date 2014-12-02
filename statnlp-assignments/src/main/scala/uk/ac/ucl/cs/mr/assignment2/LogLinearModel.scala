package uk.ac.ucl.cs.mr.assignment2

import cc.factorie.la.{GrowableSparseTensor1 => SparseVector}
import ml.wolfe.nlp.Token
import uk.ac.ucl.cs.mr.assignment2.LoadData.FeatureTemplate

/**
 * Created by fayimora on 02/12/14.
 */
class LogLinearModel(weights: SparseVector,
                     featureTemplates: Seq[FeatureTemplate]) {

  def prob(token: Token, label: String, candidate: String): Double = {
    val featVec = new SparseVector(Nil)
    for(ft <- featureTemplates) featVec += ft(token, label, candidate)
    math.exp(featVec dot weights)
  }
}
