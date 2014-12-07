package uk.ac.ucl.cs.mr.assignment2

import java.io.{FilenameFilter, File}
import ml.wolfe.nlp.{Token, Document}
import ml.wolfe.nlp.io._
import scala.collection.mutable.{ListBuffer, HashMap}
import cc.factorie.la.{GrowableSparseIndexedTensor1 => SparseVector}


object LoadData {
  lazy val stop_words = io.Source.fromFile("/Users/fayimora/Downloads/stop_words.txt").getLines().toList
  type FeatureTemplate = (Token, String, String) => Int

  def evaluate(modelTrig: Model, modelArgs: Model, trigLabels: Seq[String], argsLabels: Seq[String], docs: Iterable[Document]) {
    val trigPreds = new ListBuffer[String]
    val trigGold = new ListBuffer[String]

    val argsPreds = new ListBuffer[String]
    val argsGold = new ListBuffer[String]

    println("Building list of predictions and gold labels")
    for (doc <- docs; sent <- doc.sentences; em <- sent.ie.eventMentions) {
      val t = em.trigger
      trigPreds += modelTrig.prob(sent.tokens(t.start), trigLabels)._1
      trigGold += t.label

      for(currArg <- em.arguments) {
        val c = currArg.arg
        argsPreds += modelArgs.prob(sent.tokens(c.start), argsLabels)._1
        argsGold += c.label
      }
    }

    println("\nEvaluating model for Triggers")
    var ev = new Evaluator(trigPreds, trigGold)
    val la = "None"
    val (a, p, r, f1) = (ev.accuracy(la), ev.precision(la), ev.recall(la), ev.f1Measure(la))
    println(s"Precision: $p\nRecall: $r\nF1 Score: $f1")

    println("\nEvaluating model for Arguments")
    ev = new Evaluator(argsPreds, argsGold)
    var (a2, p2, r2, f12) = (ev.accuracy(la), ev.precision(la), ev.recall(la), ev.f1Measure(la))
    println(s"Precision: $p2\nRecall: $r2\nF1 Score: $f12")
  }

  def main(args: Array[String]) {
    val pathToBioNLP = args.lift(0).getOrElse("path-to/bionlp-train")
    val jsonFiles = new File(pathToBioNLP).listFiles(new FilenameFilter {
      override def accept(dir: File, name: String): Boolean = name.endsWith(".json")
    }).toList.take(100)

    println(s"${jsonFiles.size} files loaded")
    val vPercent = 0.20 // percentage of data for validation
    val totalNumOfFiles = jsonFiles.size
    val l = (totalNumOfFiles * vPercent).toInt // number of files to use for validation

    val validationFiles = jsonFiles.take(l) // take validation files
    val trainFiles = jsonFiles.slice(l, totalNumOfFiles)
    val trainDocs = trainFiles.map(doc => LoadBioNLP.jsonFileToWolfeDoc(doc))
    val validationDocs = validationFiles.map(doc => LoadBioNLP.jsonFileToWolfeDoc(doc))
    println(s"Files have been converted to documents")

    val trigLabelTokens = new HashMap[String, ListBuffer[Token]]()
    val argsLabelTokens = new HashMap[String, ListBuffer[Token]]()

    println("Generating necessary tables")
    for (doc <- trainDocs; sent <- doc.sentences; em <- sent.ie.eventMentions) {
      // keep track of the frequency of each trigger label
      val t = em.trigger
      val tLabel = em.trigger.label
      trigLabelTokens.getOrElseUpdate(tLabel, ListBuffer())
      trigLabelTokens(tLabel) += sent.tokens(t.start)//slice(t.start, t.end).mkString(" ")

      // keep track of the frequency of each argument label
      for(currArg <- em.arguments) {
        val cLabel = currArg.arg.label
        val c = currArg.arg
        argsLabelTokens.getOrElseUpdate(cLabel, ListBuffer())
        argsLabelTokens(cLabel) += sent.tokens(t.start) //.slice(c.start, c.end).mkString(" ")
      }
    }

    // sample None tokens
    println("Sampling None tokens")
    var x = (trigLabelTokens("None").size * 0.05).toInt
    trigLabelTokens("None") = trigLabelTokens("None").slice(0, x)

    x = (argsLabelTokens("None").size * 0.05).toInt
    argsLabelTokens("None") = argsLabelTokens("None").slice(0, x)
    println("Table generation complete")

    val trigLabels = Seq("None", "Regulation", "Positive_regulation", "Negative_regulation", "Binding",
      "Transcription", "Gene_expression", "Localization", /*"Protein_catabolism",*/ "Phosphorylation")
    val argsLabels = Seq("None", "Theme", "Cause")


    val ft = new FeatureTemplates()
    val featureTemplates = Seq(ft.template1 _, ft.template2 _, ft.template3 _, ft.template4 _)

//    var preds = new ListBuffer[String]
//    val gold = new ListBuffer[String]

//    println("It's time for a little Naive Bayes")
    val nbModelTrig = new NaiveBayes(trigLabelTokens, featureTemplates)
    val nbModelArgs = new NaiveBayes(argsLabelTokens, featureTemplates)
    println("\nEvaluating the NaiveBayes model")
    evaluate(nbModelTrig, nbModelArgs, trigLabels, argsLabels, validationDocs)
    println("*"*40)

    // high precision means that the model returned substantially more relevant results than irrelevant,
    // while high recall means that the model returned most of the relevant results.

    // ============================= Perceptron Training =============================
    val trainedWeightsTrig: SparseVector = new Perceptron(1.0, 0.01, 10, trigLabelTokens, featureTemplates).train(trigLabels)
    val trainedWeightsArgs: SparseVector = new Perceptron(1.0, 0.01, 10, argsLabelTokens, featureTemplates).train(argsLabels)

//    saveWeights(trainedWeightsTrig, trainedWeightsArgs)

//    def saveWeights(trigWeights: SparseVector, argsWeight:SparseVector): Unit = {
//      trigWeights.
//    }
    val llModelTrig = new LogLinearModel(trainedWeightsTrig, featureTemplates)
    val llModelArgs = new LogLinearModel(trainedWeightsArgs, featureTemplates)
    println("\nEvaluating the Log Likelihood model")
    evaluate(llModelTrig, llModelArgs, trigLabels, argsLabels, validationDocs)

    // test single token
    // val tk= validationDocs.head.tokens.head
    // println(s"Looking at ${tk.word}")
    // println(nbModel.prob(tk, trig Labels))

//    checkoutWordDistribution(trainDocs.take(50), Seq("None", "Theme", "Cause"))
    }

    def checkoutWordDistribution(docs: Seq[Document], labels: Seq[String]) = {
//      labels.foreach { label =>
//        println(s"===================Trigger: $label=========================")
//        for (doc <- docs;
//             sent <- doc.sentences;
//             em <- sent.ie.eventMentions
//             if em.trigger.label == label) {
//          val t = em.trigger
//          println(sent.tokens.map(_.word).slice(t.start, t.end).mkString(" "))
//        }
//      }

      labels.foreach { arg =>
        println(s"===================Argument: $arg=========================")
        for (doc <- docs;
             sent <- doc.sentences;
             em <- sent.ie.eventMentions;
             currArg <- em.arguments
             if currArg.arg.label == arg) {
          val c = currArg.arg
          //        println(em.arguments.map(_.label))
          println(sent.tokens.map(_.word).slice(c.start, c.end).mkString(" "))
        }
      }
    }




  }
