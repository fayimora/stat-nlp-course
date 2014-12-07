package uk.ac.ucl.cs.mr.assignment2

import ml.wolfe.nlp.Token

/**
 * Created by fayimora on 29/11/14.
 */
class FeatureTemplates {

  /**
   * Feature 1, take advantage of lemmas in token distribution
   */
  def template1(token: Token, label: String, e: String): Int = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    e match {
      case "trigger" => label match {
        case "None" =>
          if (LoadData.stop_words contains word) 1 else -1
        case "Regulation" =>
          if (Seq("regul", "effect", "affect", "control") contains lemma) 1 else 0
//          if (lemma == "regul") 1 else 0
        case "Positive_regulation" =>
          if (Seq("activ","increas", "induct") contains lemma) 1
          else 0
        case "Negative_regulation" =>
          if (Seq("down-regul", "inhibit", "reduc") contains lemma) 1 else 0
        case "Binding" =>
          if (Seq("bind", "interact") contains lemma) 1 else 0
        case "Phosphorylation" =>
          if (lemma == "phosphoryl") 1 else 0
        case "Localization" =>
          if (Seq("local", "secret") contains lemma) 1 else 0
        case "Protein_catabolism" =>
          if (lemma == "degrad") 1 else 0
        case "Transcription" =>
          if (Seq("transcript", "express", "level") contains lemma) 1 else 0
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


  /**
   * Feature 2, take advantage of token distribution
   */
  def template2(token: Token, label: String, e: String) = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    e match {
      case "trigger" => label match {
        case "None" => 0
        case "Regulation" =>
          if (Seq("role", "regulation") contains word) 1 else 0
        case "Positive_regulation" =>
          if (Seq("activation","induction", "overexpression", "induced", "upregulation") contains word) 1
          else 0
        case "Negative_regulation" =>
          if (Seq("inhibited", "reduced", "decreased", "blocked", "suppressed", "absense", "loss") contains word) 1 else 0
        case "Binding" =>
          if (Seq("binding", "interaction", "litigation", "ligation", "cross-linking") contains word) 1 else 0
        case "Phosphorylation" =>
          if (lemma == "phosphoryl") 1 else 0
        case "Localization" =>
          if (Seq("translocation", "localization", "secretion") contains word) 1 else 0
        case "Protein_catabolism" =>
          if (lemma == "degrad") 1 else 0
        case "Phosphorylation" =>
          if (Seq("phosphorylation", "underphosphorylated", "hyperphosphorylated", "phosphoform") contains word) 1 else -1
        case "Protein_catabolism" =>
          if (Seq("proteolysis", "degradation", "cleavage") contains word) 1 else 0
        case "Transcription" =>
          if (Seq("transcription", "levels", "synthesis") contains word) 1 else 0
        case "Gene_expression" =>
          if (Seq("expression", "production", "overexpression", "synthesis") contains word) 1 else 0
      }
      case "argument" => label match {
        case "None" => 0
        case "Theme" => 1
          if (word contains '-') 1 else 0
        case "Cause" => 1
//          if (Seq() contains word) 1 else 0 //major class
      }
    }
  }

  def template3(token: Token, label: String, e: String) = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    val pos = token.posTag
    e match {
      case "trigger" => label match {
        //case "None" =>  -2
        case "Regulation" =>
          if(pos.contains("NN") || pos.contains("VB") || pos.contains("JJ")) 1 else -1
        case "Positive_regulation" =>
          if(pos.contains("NN") || pos.contains("VB") || pos.contains("JJ")) 1 else -1
        case "Negative_regulation" =>
          if(pos.contains("NN") || pos.contains("VB") || pos.contains("JJ")) 1 else -1
        case _ => 0
      }
      case "argument" => label match {
        //case "None" => ???
        case "Theme" =>
          if (pos=="JJ" || pos=="NNS") 1 else 0
        case "Cause" =>
          if (pos=="NN" || pos=="DT") 1 else 0
        case _ => 0
      }
    }
  }

  def template4(token: Token, label: String, e: String) = {
    val word = token.word.toLowerCase
    val lemma = token.lemma.toLowerCase
    val pos = token.posTag
    e match {
      case "trigger" => label match {
        case "Regulation" =>
          if(!(Seq("EX", "RBS", "RP", "WP") contains pos)) 1 else 0
        case "Positive_regulation" =>
          if (!(Seq("RP", "EX", "RBS") contains pos)) 1 else 0
        case "Negative_regulation" =>
          if (!(Seq("EX", "WP", "PDT") contains pos)) 1 else 0
        case "Binding" =>
          if (!(Seq("WP$","RP", "EX", "WP", "PDT", "NNP") contains pos)) 1 else 0
        case "Phosphorylation" =>
          if (!(Seq("EX", "NNP", "RBR", "LS", "POS", "WP$", "WP", "RP", "PDT", "JJR", "POS", "RBS") contains pos)) 1 else 0
        case "Localization" =>
          if (!(Seq("WP", "PDT", "JJS", "WP$", "WP", "RP", "EX", "PDT", "JJR", "POS", "RBS") contains pos)) 1 else 0
        case "Protein_catabolism" =>
          if (!(Seq("JJR", "EX", "PDT", "NNP", "LS", "POS", "WP$", "RP", "WRB", "WP", "RBS", "FW", "JJS", "RBR") contains pos)) 1 else 0
        case "Transcription" =>
          if (!(Seq("PDT", "RP", "EX", "WP", "RBS") contains pos)) 1 else 0
        case "Gene_expression" =>
          if (!(Seq("PDT", "WP$") contains pos)) 1 else 0
        case _ => 0
      }
      case "argument" => label match {
        case "Cause" =>
          if (!(Seq("EX", "RBS", "RP", "WP", "WP$") contains pos)) 1 else 0
        case _ => 0
      }
    }
  }

  def template(token: Token, label: String, e: String) = {
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
