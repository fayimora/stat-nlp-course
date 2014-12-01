package uk.ac.ucl.cs.mr.assignment2

import java.io.{FilenameFilter, File}
import ml.wolfe.nlp.{Token, Document}
import ml.wolfe.nlp.io._

import scala.collection.mutable.{ListBuffer, HashMap}


object LoadData {
  lazy val stop_words = io.Source.fromFile("/Users/fayimora/Downloads/stop_words.txt").getLines().toList
  type FeatureTemplate = (Token, String, String) => Int

  def main(args: Array[String]) {
    val pathToBioNLP = args.lift(0).getOrElse("path-to/bionlp-train")
    val jsonFiles = new File(pathToBioNLP).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
    }).toList

    val vPercent = 0.30 // percentage of data for validation
    val totalNumOfFiles = jsonFiles.size
    val l = (totalNumOfFiles * vPercent).toInt // number of files to use for validation

    val validationFiles = jsonFiles.take(l) // take validation files
    val trainFiles = jsonFiles.slice(l, totalNumOfFiles)
    //    println("Showing just the head file: " + jsonFiles.head)
    //    val doc = LoadBioNLP.jsonFileToWolfeDoc(jsonFiles.head)
    val docs = trainFiles.map(doc => LoadBioNLP.jsonFileToWolfeDoc(doc))
    val validationDocs = validationFiles.take(2).map(doc => LoadBioNLP.jsonFileToWolfeDoc(doc))
    println("Files have been converted to documents")

//    val trigCounts = new HashMap[String, Int]()
//    val argsCounts = new HashMap[String, Int]()

    val trigLabelTokens = new HashMap[String, ListBuffer[Token]]()
    val argsLabelTokens = new HashMap[String, ListBuffer[Token]]()

    println("Generating necessary tables")
    for (doc <- docs; sent <- doc.sentences; em <- sent.ie.eventMentions) {
      // keep track of the frequency of each trigger label
      val t = em.trigger
      val tLabel = em.trigger.label
//      trigCounts(tLabel) = trigCounts.getOrElse(tLabel, 0) + 1
      trigLabelTokens.getOrElseUpdate(tLabel, ListBuffer())
      trigLabelTokens(tLabel) += sent.tokens(t.start)//slice(t.start, t.end).mkString(" ")

      // keep track of the frequency of each argument label
      for(currArg <- em.arguments) {
        val cLabel = currArg.arg.label
        val c = currArg.arg
//        argsCounts(cLabel) = argsCounts.getOrElse(cLabel, 0) + 1
        argsLabelTokens.getOrElseUpdate(cLabel, ListBuffer())
        argsLabelTokens(cLabel) += sent.tokens(t.start) //.slice(c.start, c.end).mkString(" ")
      }
    }
    var x = (trigLabelTokens("None").size * 0.02).toInt
    trigLabelTokens("None") = trigLabelTokens("None").slice(0, x)

    x = (argsLabelTokens("None").size * 0.05).toInt
    argsLabelTokens("None") = argsLabelTokens("None").slice(0, x)
    println("Table generation complete")

    val trigLabels = Seq("None", "Regulation", "Positive_regulation", "Negative_regulation", "Binding",
      "Transcription", "Gene_expression", "Localization", "Protein_catabolism", "Phosphorylation")
    val argsLabels = Seq("None", "Theme", "Cause")

    val ft = new FeatureTemplates()
    val featureTemplates = Seq(ft.template1 _)
    val nbModel = new NaiveBayes(trigLabelTokens, featureTemplates)
    val tk= validationDocs.head.tokens.head
    println(s"Looking at ${tk.word}")
    println(nbModel.prob(tk, trigLabels))

    val nan = 1


    }

    def checkoutWordDistribution(docs: Seq[Document], labels: Seq[String]) = {
      labels.foreach { label =>
        println(s"===================Trigger: label=========================")
        for (doc <- docs;
             sent <- doc.sentences;
             em <- sent.ie.eventMentions
             if em.trigger.label == label) {
          val t = em.trigger
          println(sent.tokens.slice(t.start, t.end).mkString(" "))
        }
      }

      labels.foreach { arg =>
        println(s"===================Argument: $arg=========================")
        for (doc <- docs;
             sent <- doc.sentences;
             em <- sent.ie.eventMentions;
             currArg <- em.arguments
             if currArg.arg.label == arg) {
          val c = currArg.arg
          //        println(em.arguments.map(_.label))
          println(sent.tokens.slice(c.start, c.end).mkString(" "))
        }
      }
    }
  }
