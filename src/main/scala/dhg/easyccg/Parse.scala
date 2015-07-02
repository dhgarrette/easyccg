package dhg.easyccg

import dhg.util._
import uk.ac.ed.easyccg.main.EasyCCG
import dhg.ccg.cat.Cat

/**
 * @author dhg
 */
object Parse {

  val WordPosCat = """(.*)\|([^|]*)\|([^|]*)""".r

  /*  
  --model            data/english/embeddings/model.train-250k 
  --evalFile         data/english/dev/gold.stagged
  --outputFile       data/english/embeddings/model.train-250k/english-dev-tagged.out
  --maxLength  50
//  --inputFormat      tokenized
//  --outputFormat     supertags

  --model            data/chinese/embeddings/model.train-250k 
  --evalFile         data/chinese/dev/gold.stagged
  --outputFile       data/chinese/embeddings/model.train-250k/chinese-dev-tagged.out
  --maxLength  50
//  --inputFormat      tokenized
//  --outputFormat     supertags
  */

  def main(args: Array[String]): Unit = {

    val (arguments, options) = parseArgs(args)

    val modelDir = options("model") // training/embeddings/model.chinese-250k
    val inputFileOpt = options.get("inputFile") // tokenized
    val evalFileOpt = options.get("evalFile") // word|pos|supertag
    val outputFileOpt = options.get("outputFile") // outputs as: word|pos|supertag
    val maxLength = options.i("maxLength", 1000)
    val numEvalSentences = options.i("numEvalSentences", Int.MaxValue)
    run(modelDir, inputFileOpt, evalFileOpt, outputFileOpt, maxLength, numEvalSentences)

  }

  def run(modelDir: String, inputFileOpt: Option[String], evalFileOpt: Option[String], outputFileOpt: Option[String], maxLength: Int, numEvalSentences: Int = Int.MaxValue) = {
    System.err.println(s"target/start dhg.easyccg.Parse --model $modelDir ${inputFileOpt.fold(""){s => s"--inputFile $s"}} ${evalFileOpt.fold(""){s => s"--evalFile $s"}} ${outputFileOpt.fold(""){s => s"--outputFile $s"}} --maxLength $maxLength --numEvalSentences $numEvalSentences")
    
    val taggerInputFile = (inputFileOpt, evalFileOpt) match {
      case (Some(inputFile), None) => inputFile
      case (None, Some(evalFile)) =>
        val fakedInputFile = java.io.File.createTempFile(File(evalFile).getName.rsplit("\\.", 2).head, ".in")
        writeUsing(fakedInputFile) { w =>
          File(evalFile).readLines.take(numEvalSentences).foreach { line =>
            w.wl(line.splitWhitespace.map { case WordPosCat(w, p, c) => w }.mkString(" "))
          }
        }
        fakedInputFile.getAbsolutePath
      case _ => sys.error("specify exactly one of --inputFile or --evalFile")
    }

    val taggerOutputFile = outputFileOpt.getOrElse(java.io.File.createTempFile(File(taggerInputFile).getName.rsplit("\\.", 2).head, ".out").getAbsolutePath)

    System.err.println(f"Preparing to parse ${File(taggerInputFile).readLines.size} sentences")
    val easyccgArgs = Array(
      "--model", modelDir,
      "--inputFile", taggerInputFile,
      "--inputFormat", "tokenized",
      "--outputFormat", "supertags",
      "--outputFile", taggerOutputFile,
      "--timing",
      "--maxLength", maxLength.toString)
      println(f"Running target/start uk.ac.ed.easyccg.main.EasyCCG ${easyccgArgs.mkString(" ")}")
    time("Running EasyCCG", EasyCCG.main(easyccgArgs), System.err.println)
    if (outputFileOpt.isEmpty) File(taggerOutputFile).readLines.foreach(println)

    //val taggerOutputFile = outputFileOpt.get
    evalFileOpt.foreach { evalFile =>
      val accuracy =
        (File(evalFile).readLines zip /*Safe*/ File(taggerOutputFile).readLines).zipWithIndex.flatMap {
          case ((goldLine, outputLine), lineNum) =>
            val goldTokens = goldLine.splitWhitespace
            val outputTokens = outputLine.splitWhitespace
            //if (outputTokens.nonEmpty && goldTokens.size != outputTokens.size) println(f"[$lineNum]  outputLine")
            if (goldTokens.size == outputTokens.size) {
              (goldTokens zipSafe outputTokens).map {
                case (WordPosCat(gw, gp, gc), WordPosCat(mw, mp, mc)) =>
                  assert(gw == mw, f"words don't match: [${lineNum + 1}]  gold=$gw, model-output=$mw")
                  if (gc == mc) 1 else 0
              }
            }
            else Vector.fill(outputLine.splitWhitespace.length)(0)
        }.avg

      println(f"Supertag accuracy from easyccg parser: ${accuracy * 100}%.2f")
    }
  }
}

