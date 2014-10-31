package uk.ac.ucl.cs.mr.assignment1

import java.io.File

import ml.wolfe.nlp.{Document, Sentence, Token, _}
import org.json4s.NoTypeHints
import org.json4s.native.Serialization
import org.json4s.native.Serialization.{read, write}
import uk.ac.ucl.cs.mr.assignment1.Assignment1.LanguageModel
import scala.io.{Source, Codec}

/**
 * @author Sebastian Riedel
 */
object Assignment1Util {
  /* Type aliases */
  type Counts = Map[NGram,Double]
  type NGram = Seq[String]

  /**
   * Converts a file into a tokenized and sentence segmented document
   * @param file text file to convert
   * @return Document of sentences and tokens.
   */
  def toDocument(file: File): Document = {
    val content = Source.fromFile(file)(Codec("ISO8859-1")).getLines().toArray
    val text = content.foldLeft("")(_ + " " + _)
    val pipeline = SentenceSplitter andThen TokenSplitter
    pipeline(text)
  }

  /**
   * Generic method to get n-grams from a sentence
   * @param sentence sentence to n-grams from
   * @param n n-gram order
   * @return n-grams in sentence
   */
  def ngramsInSentence(sentence: Sentence, n: Int): Seq[NGram] = {
    sentence.tokens.map(_.word).sliding(n).toSeq
    // sentence.tokens.sliding(n).toSeq
  }

  /**
   * Get all descendant files in the directory specified by f.
   * @param f the parent directory.
   * @return all files in the directory, recursive.
   */
  def recursiveListFiles(f: File): Array[File] = {
    val these = f.listFiles
    these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
  }

  /**
   * Add counts to an immutable map
   * @param counts map with counts
   * @param ngram ngram to increase count
   * @param count count to add
   * @return new map with count of ngram increased by count.
   */
  def addNgramCount(counts: Counts, ngram: NGram, count: Double = 1.0): Counts = {
    counts + (ngram -> counts.getOrElse(ngram, count))
  }

  /**
   * Take two count maps, and add their counts
   * @param countsMany first count map
   * @param countsFew second count map
   * @return a map that maps each key in the union of keys of counts1 and counts2 to the sum of their counts.
   */
  def addNgramCounts(countsMany: Counts, countsFew: Counts): Counts = {
    countsMany ++ countsFew.map { case (k, v) => k -> (v + countsMany.getOrElse(k, 0.0)) }
  }

  /**
   * Collect n-gram counts of a given order in the document
   * @param document the document to get n-gram counts from
   * @param n the n-gram order.
   * @return a n-gram to count in document map
   */
  def getNGramCounts(document: Document, n: Int): Counts = {
    val doc: Seq[Seq[NGram]] = document.sentences.map(s => ngramsInSentence(s, n))
    doc.flatMap(_.map(ng => addNgramCount(Map.empty, ng))) reduce addNgramCounts
  }

  /**
   * For a given n-gram count map get the counts for n-1 grams.
   * @param counts the n-gram counts
   * @return the n-1 gram counts.
   */
  def getNMinus1Counts(counts: Counts): Counts = ???



  //will be provided soon, so nothing to do for you here
  def serialize(lm: LanguageModel, ngrams: Seq[NGram]) = {
    val dictionary: Seq[String] = ???

    for {
      ngram <- ngrams
      word <- dictionary
    } {
      lm.prob(word, ngram)
      //do serialization here

      //use read and write
    }
  }
}








