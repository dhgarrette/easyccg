package dhg.easyccg

import dhg.util._
import uk.ac.ed.easyccg.main.EasyCCG
import dhg.ccg.data._
import java.io.BufferedWriter
import dhg.ccg.parse._
import dhg.ccg.cat._

/**
 * @author dhg
 */
object Train {

  /*
   ./do_training.sh my_model turian experiments/wsj_train/ experiments/wsj00/
   
   --lang english
   --embeddingsDir data/english/embeddings
   --numTrainTok 001k
   --trainDir data/english/train-001k
   --evalDir data/english/dev
   --trainFiles 0200-2199
   --evalFiles 0000-0099

   --lang chinese
   --embeddingsDir data/chinese/embeddings
   --numTrainTok all
   --trainDir data/chinese/train-all
   --evalDir data/chinese/dev
   --trainFiles 1-815,1001-1136,2000-2980
   --evalFiles 900-931,1148-1151,2981-3029
   // chinese test is: 816-885,1137-1147,3030-3145
   */

  def main(args: Array[String]): Unit = {

    val (arguments, options) = parseArgs(args)

    val lang = options("lang")
    val embeddingsDir = options("embeddingsDir") // should contain a file `embeddings.raw`;  be sure to run `./splitEmbeddings.sh` on this folder
    val numTrainTok = options("numTrainTok") match { case "all" => Int.MaxValue; case v => v.replaceAll("k$", "000").toInt } // e.g.: `250k` or `all`
    val trainDir = options("trainDir") // will create a file `gold.stagged` of the form `word|pos|supertag`, without indices
    val evalDir = options("evalDir") //   will create a file `gold.stagged` of the form `word|pos|supertag`, without indices
    val trainFiles = options("trainFiles")
    val evalFiles = options("evalFiles")

    System.err.println(f"lang: $lang")
    System.err.println(f"embeddingsDir: $embeddingsDir")
    System.err.println(f"numTrainTok: $numTrainTok")
    System.err.println(f"trainDir: $trainDir")
    System.err.println(f"evalDir: $evalDir")
    System.err.println(f"trainFiles: $trainFiles")
    System.err.println(f"evalFiles: $evalFiles")

    val reader: CcgbankReader = lang match {
      case "chinese" => new ChineseCcgTreeBankReader
      case "english" => new EnglishCcgTreeBankReader
      //case "italian" => new ItalianCcgTreeBankReader
    }

    val trainData = time("read train data", for { fileNum <- RangeString(trainFiles); trees <- reader.readFileTrees(fileNum).toSeq; t <- trees } yield t, System.err.println)
    val evalData = time("read dev data", for { fileNum <- RangeString(evalFiles); trees <- reader.readFileTrees(fileNum).toSeq; t <- trees } yield t, System.err.println)

    System.err.println(f"trainData: ${trainData.size} sentences, ${trainData.sumBy(_.length)} tokens")
    System.err.println(f"evalData: ${evalData.size} sentences, ${evalData.sumBy(_.length)} tokens")

    writeUsing(File(trainDir, "gold.stagged")) { f => writeSentences(trainData, f, numTrainTok) }
    writeUsing(File(evalDir, "gold.stagged")) { f => writeSentences(evalData, f, Int.MaxValue) }

    makeUnaryRuleFile(trainData, trainDir, lang)

    
    if(!File(embeddingsDir, "embeddings.words").exists) {
      // ./training/splitEmbeddings.sh data/english/embeddings
      time("splitting embeddings", Subprocess("/bin/sh", File(sys.env("HOME"), "workspace/easyccg/training/splitEmbeddings.sh").getAbsolutePath, embeddingsDir).call(), System.err.println)
    }

    val jobName = File(trainDir).name
    val doTrainingArgs = Vector(File(sys.env("HOME"), "workspace/easyccg/training/do_training.sh").getAbsolutePath, jobName,
      File(sys.env("HOME"), "workspace/easyccg", embeddingsDir).getAbsolutePath,
      File(sys.env("HOME"), "workspace/easyccg", trainDir).getAbsolutePath,
      File(sys.env("HOME"), "workspace/easyccg", evalDir).getAbsolutePath)
    println(s"cd ${File(sys.env("HOME"), "workspace/easyccg/training")}; ${doTrainingArgs.mkString(" ")}")
    Subprocess("/bin/sh", doTrainingArgs: _*).call()

    val supertaggerAccuracy = File(embeddingsDir, s"train.${jobName}/log").readLines.toVector.takeRight(2).head.splitWhitespace.last.toDouble
    println(f"Eval set supertagger accuracy: ${supertaggerAccuracy * 100}%.2f")

  }

  def makeUnaryRuleFile(trainData: Vector[CcgTree], trainDir: String, lang: String) = {
    time("count unary rule", {
      def traverse(t: CcgTree, i: Int, j: Int): Vector[(Cat, Cat)] = t match {
        case CcgLeaf(cat, word, _) => Vector()
        case CcgUnode(cat, s) => (s.cat.noIndices -> cat.noIndices) +: traverse(s, i, j)
        case CcgBinode(cat, l, r) => traverse(l, i, i + l.length) ++ traverse(r, i + l.length, j)
      }
      val unaryRuleCounts = trainData.flatMap(t => traverse(t, 0, t.length)).counts.desc // pairs (child, parent)
      unaryRuleCounts.foreach { case ((child, parent), count) => println(f"$count%5d   $child   $parent") }

      writeUsing(File(trainDir, "unaryRules")) { f =>
        val unaryRules = lang match {
          case "chinese" => unaryRuleCounts.collect { case ((child, parent), count) if count >= 10 => (child, parent) }
          case "english" => Vector(
            ("""N""", """NP"""),
            ("""(S[pss]\NP)""", """(N\N)"""),
            ("""(S[ng]\NP)""", """(N\N)"""),
            ("""(S[adj]\NP)""", """(N\N)"""),
            ("""(S[dcl]/NP)""", """(N\N)"""),
            ("""(S[to]\NP)""", """(S/S)"""),
            ("""(S[pss]\NP)""", """(S/S)"""),
            ("""(S[ng]\NP)""", """(S/S)"""),
            ("""NP""", """(S[X]/(S[X]\NP))"""),
            ("""NP""", """((S[X]\NP)\((S[X]\NP)/NP))"""),
            ("""PP""", """((S[X]\NP)\((S[X]\NP)/PP))"""))
        }
        unaryRules.foreach { case (child, parent) => f.wl(f"$child    $parent") }
      }
    }, System.err.println)
  }

  def writeSentences(trees: Vector[CcgTree], writer: BufferedWriter, tokCount: Int): Unit = {
    trees.map(_.wordposcats).takeSub(tokCount).foreach { sentence =>
      writer.wl(sentence.map { case (w, p, c) => f"$w|$p|${c.noIndices}" }.mkString(" "))
    }
  }
}

