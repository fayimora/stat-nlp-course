import ml.wolfe.nlp.{CharOffsets, Token}

val ls = "the is nlp crap".split(" ")
val n = 2
val terminators = for (i<- 0 to n-1) yield new Token("<s>", new CharOffsets(-1, -1))
((terminators ++: ls) ++ terminators).toSeq.foreach(println)
/*************************************************/
type NGram = Seq[String]
type Sentence = Seq[NGram]
type Counts = Map[NGram,Double]
type Document = Seq[Sentence]
val inputs = Seq("my great name is Fayi","I hate this coursework", "I have a lot to do", "Does Fayi have a lot to do")
val tokenisedInputs: Sentence = inputs.map(_.split("\\s").toSeq)
val ngramedInputs = tokenisedInputs.map(ni => ngramsInSentence(ni, 3))
ngramedInputs.head.foreach(println)
val counts = for(sentence <- ngramedInputs; ng <- sentence) yield addNgramCount(Map.empty, ng)
counts.foreach(println)
val accCounts = counts.reduce(addNgramCounts)
accCounts.keys.mkString("\n")
val temp: Seq[NGram] = accCounts.keys.toList.map(_.drop(1))
temp.groupBy(t=>t).mapValues(_.size.toDouble).mkString("\n")
//addNgramCounts(temp.groupBy(t=>t).mapValues(_.size.toDouble), Map.empty[NGram,Double])
Seq(1,2,4,3,6,112).dropRight(1)
//val tempCounts: Map[NGram, Double] = Map.empty
//(for (ngram <- accCounts.keys) yield ngram.drop(1)).mkString("\n")
//val ngramCounts = getNGramCounts(ngramedInputs, 2)
//ngramCounts.foreach(println)
//ngramCounts.keys//.map(_(2)).foreach(println)
val minus1Counts = getNMinus1Counts(accCounts)
minus1Counts.mkString("\n")
def ngramsInSentence(sentence: Seq[String], n: Int) = {
  val startTerminators = for(i <- 1 to n-1) yield "<s>"
  val endTerminators = for(i <- 1 to n-1) yield "</s>"
  ((startTerminators++: sentence) ++ endTerminators).sliding(n).toSeq
//  sentence.sliding(n).toSeq
}
def addNgramCount(counts: Counts, ngram: NGram, count: Double = 1.0): Counts = {
  counts + (ngram -> counts.getOrElse(ngram, count))
}
def addNgramCounts(countsMany: Counts, countsFew: Counts): Counts = {
  countsMany ++ countsFew.map { case (k, v) => k -> (v + countsMany.getOrElse(k, 0.0)) }
}
def getNGramCounts(document: Document, n: Int) = {
  val doc: Seq[Seq[NGram]] = document.flatMap(_.map(s => ngramsInSentence(s, n)))
  doc.flatMap(_.map(ng => addNgramCount(Map.empty, ng))) reduce addNgramCounts
}
def getNMinus1Counts(counts: Counts) = {
  counts.keys.toList.map(_.dropRight(1))
}
/*
def toNgrams2(sentence: Seq[String], n: Int) = {
  def ngram(currSentence: Seq[String], grams: Seq[Seq[String]]): Seq[Seq[String]] = {
    if(currSentence.size <= n){
      grams
    } else {
      val group: Seq[String] = currSentence.slice(0, n).toSeq
      val newGrams: Seq[Seq[String]] = grams :+ group
      ngram(currSentence.tail, newGrams)
    }
  }
  ngram(sentence, List.empty)
}
*/