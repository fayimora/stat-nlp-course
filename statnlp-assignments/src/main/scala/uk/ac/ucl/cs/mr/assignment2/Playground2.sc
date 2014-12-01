import cc.factorie.la.GrowableSparseTensor1

val vec = new GrowableSparseTensor1(Nil)
for(i<- 1 to 20) vec += (i, 0)
vec
vec += (12, 1)
vec += (22, 1)
vec += (23, 1)
vec.sum
List(0, 43, 1, 398, 10).zipWithIndex.maxBy(_._1)._2

def template1(word: String, label: String, e: String) = e match {
  case "trigger" => label match {
    case "None" => "Welcome to trigger land"
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
    case "None" => "Yaay it got to argument"
    case "Theme" => ???
    case "Cause" => ???
    case _ => ???
  }
}

template1("Something", "None", "trigger")