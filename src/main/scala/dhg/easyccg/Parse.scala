package dhg.easyccg

import dhg.util._
import uk.ac.ed.easyccg.main.EasyCCG

/**
 * @author dhg
 */
object Parse {

  val WordPosCat = """(.*)\|([^|]*)\|([^|]*)""".r

  /*  
  --model        data/chinese/chinese-embeddings/model.chinese-250k 
  --evalFile     data/chinese/dev_small/gold.stagged  
  --outputFile   data/chinese/chinese-250k/chinese-dev-tagged.out
  --inputFormat  tokenized  
  --outputFormat supertags  
  */

  def main(args: Array[String]): Unit = {

    val (arguments, options) = parseArgs(args)

    val modelDir = options("model") // training/chinese-embeddings/model.chinese-250k
    val inputFileOpt = options.get("inputFile") // tokenized
    val evalFileOpt = options.get("evalFile") // word|pos|supertag
    val outputFileOpt = options.get("outputFile") // outputs as: word|pos|supertag

    val taggerInputFile = (inputFileOpt, evalFileOpt) match {
      case (Some(inputFile), None) => inputFile
      case (None, Some(evalFile)) =>
        val fakedInputFile = java.io.File.createTempFile(File(evalFile).getName.rsplit("\\.", 2).head, ".in")
        writeUsing(fakedInputFile) { w =>
          File(evalFile).readLines.foreach { line =>
            w.wl(line.splitWhitespace.map { case WordPosCat(w, p, c) => w }.mkString(" "))
          }
        }
        fakedInputFile.getAbsolutePath
      case _ => sys.error("specify exactly one of --inputFile or --evalFile")
    }

    val taggerOutputFile = outputFileOpt.getOrElse(java.io.File.createTempFile(File(taggerInputFile).getName.rsplit("\\.", 2).head, ".out").getAbsolutePath)

    System.err.println(f"Preparing to parse ${File(taggerInputFile).readLines.size} sentences")
    EasyCCG.main(Array(
      "--model", modelDir,
      "--inputFile", taggerInputFile,
      "--inputFormat", "tokenized",
      "--outputFormat", "supertags",
      "--outputFile", taggerOutputFile))
    if (outputFileOpt.isEmpty) File(taggerOutputFile).readLines.foreach(println)

    evalFileOpt.foreach { evalFile =>
      val accuracy =
        (File(evalFile).readLines zipSafe File(taggerOutputFile).readLines).zipWithIndex.flatMap {
          case ((evalLine, outputLine), lineNum) =>
            (evalLine.splitWhitespace zipSafe outputLine.splitWhitespace).map {
              case (WordPosCat(gw, gp, gc), WordPosCat(mw, mp, mc)) =>
                assert(gw == mw, f"words don't match: [${lineNum + 1}]  gold=$gw, model-output=$mw")
                if (gc == mc) 1 else 0
            }
        }.avg

      println(f"accuracy: ${accuracy * 100}%.2f")
    }
  }

}

