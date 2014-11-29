package uk.ac.ucl.cs.mr.assignment1

import ml.wolfe.nlp.Document
import java.io.File

/**
 * @author Sebastian Riedel
 */
object Assignment1 {

  import uk.ac.ucl.cs.mr.assignment1.Assignment1Util._

  trait LanguageModel {
    def order:Int
    def prob(word: String, history: NGram): Double
  }

  def perplexity(lm: LanguageModel, documents:Iterator[Document]): Double = {
    val docs = documents.toList
    //TODO: refactor duplication later
    val ngrams = docs.flatMap(_.sentences.flatMap(s => ngramsInSentence(s, lm.order)))
//    val vocab = docs.flatMap(_.sentences.flatMap(_.tokens.map(_.word))).size//toSeq.distinct
    val prod = ngrams.map(ng => math.log(lm.prob(ng.last, ng.slice(0, ng.size-1)))).sum // take log probs and sum
//    val pp = math.pow(prod, 1/vocab)
    val pp = math.exp(prod * (-1.0/ngrams.size))
    pp
  }

  class ConstantLM(vocabSize: Int) extends LanguageModel {
    val order = 1
    def prob(word: String, history: NGram) = {
      //val count = history.count(_==word)
      1 / vocabSize.toDouble
    }
  }

  class NgramLM(val countsN: Counts, val countsNMinus1: Counts, val order: Int) extends LanguageModel {
    def prob(word: String, history: NGram) = {
      val count1 = countsN.getOrElse(history :+ word, 0.0)
      val count2 = countsNMinus1.getOrElse(history, 1.0)
      count1 / count2
    }
  }

  class AddOneLM(val ngramLM: NgramLM, vocabSize: Int, eps: Double = 1.0) extends LanguageModel {
    def order = ngramLM.order
    lazy val cc = ngramLM.countsN.values.sum
    def prob(word: String, history: NGram) = {
      //makes a new ngram equal to the history plus the word.
      val fullGram = history :+ word

      val gramCount = ngramLM.countsN.getOrElse(fullGram,0.0)
      if (ngramLM.order == 1)
        (gramCount + eps) / (cc + vocabSize)
      else
        (gramCount + eps) / (ngramLM.countsNMinus1.getOrElse(history, 1.0) + vocabSize)
      //add vocab size to total probability count to normalise
//      (ngramLM.countsN.getOrElse(fullGram, 0.0)+eps) / (ngramLM.countsNMinus1.getOrElse(history, 1.0)+vocabSize)
    }
  }

  class GoodTuringLM(ngramLM: NgramLM) extends LanguageModel {
    def order = ngramLM.order
    def prob(word: String, history: NGram) = ???
  }

  def trainNgramLM(train:Seq[Document], order:Int): NgramLM = {
    val counts1 = train.map(t => getNGramCounts(t, order)) reduce addNgramCounts
    val counts2: Counts = countsTableCache.getOrElse(order-1, getNMinus1Counts(counts1))
    new NgramLM(counts1, counts2, order)
  }

  def testLM(lm: LanguageModel, vocab: NGram, history: Seq[String], testDocuments: Iterator[Document]) = {

    val pp = perplexity(lm, testDocuments)

    //===== Generate most likely sentence ====
//    val newLM = (lm.asInstanceOf[AddOneLM]).ngramLM
//    val kss = newLM.countsN.keys.toList
//    val probMap: Map[NGram, Double] =
//      newLM.countsN.keys.map(ng => (ng, newLM.prob(ng.last, ng.slice(0, ng.size-1)))).toList.toMap
//
//    //find key with <s> in starting position
//    var start = probMap.keys.filter(ng => ng.head.trim=="<s>").maxBy(ng => probMap.getOrElse(ng,0.0))
//    println(start.map(_.toString).mkString(" "))
//    (0 to 10).foreach{i =>
//      start = probMap.keys.filter(_.head==start.last).maxBy(ng => probMap.getOrElse(ng,0.0))
//      println(start.map(_.toString).mkString(" "))
//    }
    //========================================

//    vocab.map(w => lm.prob(w, history)).sum
      println(s"Perplexity is $pp")
//    println(s"The probability of study is $p")
  }

  var countsTableCache: Map[Int, Counts] = Map.empty
  def main(args: Array[String]) {
    println("Hello, world!")
    //todo

    // Load train data
    val folder = "/Users/fayimora/Downloads/normal/P/"
//    val folder = "/Users/fayimora/Misc/SNLP CW1 Dataset/ALL/"
    def toF (nums: Range) = nums.map(n=> new File(folder+"P%02d".format(n)))
    val trainFiles = toF(79 to 79)// ++ toF(0 to 4)
    val testFiles = toF(5 to 6)

    val allFiles = trainFiles.map(recursiveListFiles _).flatten
    val files =  allFiles.filter(_.getName.endsWith(".txt"))
    val documents: Seq[Document] = files.map(toDocument _)
//    val documents = getDocuments(tempFiles)
    val vocab = documents.flatMap(_.sentences.flatMap(_.tokens.map(_.word))).distinct
    var order = 3

    var testSet = testFiles.map(recursiveListFiles _).flatten
    testSet = testSet.filter(_.getName.endsWith(".txt"))
    val testDocuments = testSet.map(toDocument _)


    val history = Seq("we")

    println("=================== ConstantLM =========================")
    val cLM = new ConstantLM(vocab.size)
    testLM(cLM, vocab, history, testDocuments.toIterator)
//    val cLM = new ConstantLM(vocab.size)
//    val p = cLM.prob("study", vocab)
//    var pp = perplexity(cLM, testDocuments.toIterator)
//    vocab.map(w => cLM.prob(w, history)).sum
//    println(s"Perplexity is $pp")
//    println(s"The probability of study is $p")



//    val ss = Seq("uni", "bi", "tri")
//    val ss = Seq("tri", "bi", "uni")
//    for((tag, o) <- ss.zipWithIndex) {
      for(tag <- (1 to 3).reverse){
        val or = tag
        println(s"=================== $tag-gram LM =========================")
        val ngramLM = trainNgramLM(documents, or)
        countsTableCache = countsTableCache + (or-1 -> ngramLM.countsNMinus1) // cache nMinus1Counts for the next model
        val smoothLM = new AddOneLM(ngramLM, vocab.size)

//        val sum = vocab.map(w => ngramLM.prob(w, history)).sum
//        println(s"total prob given we is $sum")
        testLM(smoothLM, vocab, history, testDocuments.toIterator)
      }
//    println("=================== uni-gram LM =========================")
//    order = 1
//    var ngramLM = trainNgramLM(documents, order)
//    var smoothLM = new AddOneLM(ngramLM, vocab.size)
//    testLM(smoothLM, vocab, history, testDocuments.toIterator)
//
//
//    println("=================== bi-gram LM =========================")
//    order = 2
//    ngramLM = trainNgramLM(documents, order)
//    smoothLM = new AddOneLM(ngramLM, vocab.size)
//    testLM(smoothLM, vocab, history, testDocuments.toIterator)
//
//    println("=================== tri-gram LM =========================")
//    order = 3
//    ngramLM = trainNgramLM(documents, order)
//    smoothLM = new AddOneLM(ngramLM, vocab.size)
//    testLM(smoothLM, vocab, history, testDocuments.toIterator)

    // check p(want | if, we)
//    val p = ngramLM.prob("want", history)
//    println(s"Probability is $p")


    //The probability of every word in the vocabulary given history should converge to 1
//    val histStr = history.mkString(",")
//    var sum = 0.0
//    for(word <- vocab){
//      val p = ngramLM.prob(word, history).toDouble
//      sum = sum+p
//      println(s"P($word|$histStr) = $p")
//    }


//    val smoothLM = new AddOneLM(ngramLM, vocab.size)
//    pp = perplexity(smoothLM, testDocuments.toIterator)
//    println(s"Order is $order")
////    val sum = vocab.map(w => ngramLM.prob(w, history)).sum
////    println(s"Sum of all conditionals is $sum")
//    println(s"Perplexity is $pp")


//
//    println(s"Vocabulary size is ${vocabulary.size}")
//    println(s"Unique tokens count is ${vocabulary.distinct.size}")
//    val tokens = documents.flatMap(_.sentences.flatMap(_.tokens.map(_.word)))
//    val p = cLM.prob("study", tokens)
//    val sum = vocabulary.distinct.map(w=>cLM.prob(w, tokens)).reduce(_+_)
//    val counts = getNGramCounts(documents.head, 1)
//    println(s"The ConstantLM probability is $p")


  }
}