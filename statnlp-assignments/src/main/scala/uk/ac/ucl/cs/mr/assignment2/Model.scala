package uk.ac.ucl.cs.mr.assignment2

import ml.wolfe.nlp.Token

/**
 * Created by fayimora on 06/12/14.
 */
trait Model {
  def prob(t: Token, labels: Seq[String]): (String, Double)
}
