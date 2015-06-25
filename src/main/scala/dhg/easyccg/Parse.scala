package dhg.easyccg

import dhg.util._
import uk.ac.ed.easyccg.main.EasyCCG

/**
 * @author dhg
 */
object Parse {

  val WordPosCat = """(.*)\|([^|]*)\|([^|]*)""".r

  /*  
  --model        data/english/embeddings/model.train-250k 
  --goldFile     data/english/dev/gold.stagged
  --outputFile   data/english/embeddings/model.train-250k/english-dev-tagged.out
  --inputFormat  tokenized
  --outputFormat supertags

  --model        data/chinese/embeddings/model.train-250k 
  --goldFile     data/chinese/dev/gold.stagged
  --outputFile   data/chinese/embeddings/model.train-250k/chinese-dev-tagged.out
  --inputFormat  tokenized
  --outputFormat supertags
  */

  def main(args: Array[String]): Unit = {

    val (arguments, options) = parseArgs(args)

    val modelDir = options("model") // training/embeddings/model.chinese-250k
    val inputFileOpt = options.get("inputFile") // tokenized
    val goldFileOpt = options.get("goldFile") // word|pos|supertag
    val outputFileOpt = options.get("outputFile") // outputs as: word|pos|supertag

    val taggerInputFile = (inputFileOpt, goldFileOpt) match {
      case (Some(inputFile), None) => inputFile
      case (None, Some(goldFile)) =>
        val fakedInputFile = java.io.File.createTempFile(File(goldFile).getName.rsplit("\\.", 2).head, ".in")
        writeUsing(fakedInputFile) { w =>
          File(goldFile).readLines.foreach { line =>
            w.wl(line.splitWhitespace.map { case WordPosCat(w, p, c) => w }.mkString(" "))
          }
        }
        fakedInputFile.getAbsolutePath
      case _ => sys.error("specify exactly one of --inputFile or --goldFile")
    }

    val taggerOutputFile = outputFileOpt.getOrElse(java.io.File.createTempFile(File(taggerInputFile).getName.rsplit("\\.", 2).head, ".out").getAbsolutePath)

    System.err.println(f"Preparing to parse ${File(taggerInputFile).readLines.size} sentences")
    time("Running EasyCCG", {
      EasyCCG.main(Array(
        "--model", modelDir,
        "--inputFile", taggerInputFile,
        "--inputFormat", "tokenized",
        "--outputFormat", "supertags",
        "--outputFile", taggerOutputFile))
    }, System.err.println)
    if (outputFileOpt.isEmpty) File(taggerOutputFile).readLines.foreach(println)

    //val taggerOutputFile = outputFileOpt.get
    goldFileOpt.foreach { goldFile =>
      val accuracy =
        (File(goldFile).readLines zip /*Safe*/ File(taggerOutputFile).readLines).zipWithIndex.flatMap {
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

      println(f"accuracy: ${accuracy * 100}%.2f")
    }
  }

}

