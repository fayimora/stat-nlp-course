package uk.ac.ucl.cs.mr.assignment2

import ml.wolfe.nlp.Token

/**
 * Created by fayimora on 29/11/14.
 */
class FeatureTemplates {

  def template1(token: Token, label: String, e: String): Int = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    e match {
      case "trigger" => label match {
        case "None" =>
          if (LoadData.stop_words contains word) 1 else 0
        case "Regulation" =>
          if (lemma == "regul") 1 else 0
        case "Positive_regulation" =>
          if (Seq("resulted", "increase") contains word) 1
          else 0
        case "Negative_regulation" =>
          if (lemma == "down-regul") 1 else 0
        case "Binding" =>
          if (lemma == "bind") 1 else 0
        case "Phosphorylation" =>
          if (lemma == "phosphoryl") 1 else 0
        case "Localization" =>
          if (lemma == "local" || lemma == "secret") 1 else 0
        case "Protein_catabolism" =>
          if (lemma == "degrad") 1 else 0
        case "Transcription" =>
          if (lemma == "transcript") 1 else 0
        case "Gene_expression" =>
          if(lemma == "express") 1 else 0
      }
      case "argument" => label match {
        case "None" => if (LoadData.stop_words contains word) 1 else 0
        case "Theme" =>
          if(lemma == "express") 1 else 0
//        case "Cause" => ???
        case _ => 1 // TODO: THIS IS A HACK!!!
      }
    }
  }

  def template2(token: Token, label: String, e: String) = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    e match {
      case "trigger" => label match {
        case "None" => ???
        case "Regulation" => ???
        case "Positive_regulation" => ???
        case "Negative_regulation" => ???
        case "Binding" => ???
        case "Phosphorylation" => ???
        case "Localization" => ???
        case "Protein_catabolism" => ???
        case "Transcription" => ???
        case "Gene_expression" => ???
        case _ => ???
      }
      case "argument" => label match {
        case "None" => ???
        case "Theme" => ???
        case "Cause" => ???
      }
    }
  }
}
