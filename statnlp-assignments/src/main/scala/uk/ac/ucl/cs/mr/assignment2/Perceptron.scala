package uk.ac.ucl.cs.mr.assignment2

import cc.factorie.la.{GrowableSparseIndexedTensor1 => SparseVector}
import ml.wolfe.nlp.Token
import uk.ac.ucl.cs.mr.assignment2.LoadData.FeatureTemplate

import scala.collection.mutable.{ListBuffer, HashMap}

/**
 * Created by fayimora on 01/12/14.
 */
class Perceptron(n: Double,
                 iterations: Int,
                 labelTokens: HashMap[String, ListBuffer[Token]],
                 featureTemplates: Seq[FeatureTemplate]) {

  val candidate = if(labelTokens.keys.size > 3) "trigger" else "argument"

  def train(labels: Seq[String]): SparseVector = {

    var weights = new SparseVector(Nil)
    for(i <- 1 to 10) weights += (i, n)

    val data = for((l, toks) <- labelTokens.toSeq; t <- toks) yield (t, l)
    var converged = false
    for(iter <- 1 to iterations;
        (tok, label) <- data if !converged) {
      val llm = new LogLinearModel(weights, featureTemplates)
      val classes = labelTokens.keys.toList
      val idx = classes.map(c =>  llm.prob(tok, label, candidate)).zipWithIndex.maxBy(_._1)._2
      val predLabel = classes(idx)

      if(predLabel != label) {
//        val vec1 = new SparseVector(Nil)
//        for (ft <- featureTemplates) vec1 += ft(tok, predLabel, candidate)
//        val vec2 = new SparseVector(Nil)
//        for (ft <- featureTemplates) vec1 += ft(tok, label, candidate)

        val diff = featureTemplates.map(_(tok, predLabel, candidate)).
          zip(featureTemplates.map(_(tok, label, candidate))).map(tup => tup._1 - tup._2)
//        reduce( (a,b) => (a._1+a._2)*(b._1+b._2) )

        val vec = new SparseVector(Nil)
        for((x,i) <- diff.zipWithIndex) vec += (i, x)
        weights = (weights + vec).asInstanceOf[SparseVector]
      } else converged = true
    }
    weights
  }

}
