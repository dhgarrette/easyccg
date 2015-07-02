package dhg.easyccg

import java.io.BufferedWriter

import dhg.ccg.cat._
import dhg.ccg.data._
import dhg.ccg.parse._
import dhg.easyccg.Common.StripCatOuterParens
import dhg.util._

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
    val numTrainTok = options.get("numTrainTok") match { case Some("all") | None => Int.MaxValue; case Some(v) => v.replaceAll("k$", "000").toInt } // e.g.: `250k` or `all`
    val minUnaryRuleCount = options.i("minUnaryRuleCount", 10)
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

    val reader: TreeBankReader = new RebankingTreeBankReader(new SimpleRebanker(RebankRules.standard), lang match {
      case "chinese" => new ChineseCcgTreeBankReader
      case "english" => new EnglishCcgTreeBankReader
      //case "italian" => new ItalianCcgTreeBankReader
    })

    val trainData = time("read train data", new StoppingCountTreeIterator(for { fileNum <- RangeString(trainFiles).iterator; trees <- reader.readFileTrees(fileNum).toSeq; t <- trees } yield t, numTrainTok).toVector.init, System.err.println)
    System.err.println(f"trainData: ${trainData.size} sentences, ${trainData.sumBy(_.length)} tokens")
    writeUsing(File(trainDir, "gold.stagged")) { f => writeSentences(trainData, f) }

    val evalFile = File(evalDir, "gold.stagged")
    if (!evalFile.exists) {
      val evalData = time("read dev data", for { fileNum <- RangeString(evalFiles); trees <- reader.readFileTrees(fileNum).toSeq; t <- trees } yield t, System.err.println)
      System.err.println(f"evalData: ${evalData.size} sentences, ${evalData.sumBy(_.length)} tokens")
      writeUsing(evalFile) { f => writeSentences(evalData, f) }
    }
    else
      System.err.println(f"Eval data already exists; skipping generation.  ${evalFile}")

    if (!File(embeddingsDir, "embeddings.words").exists) {
      // ./splitEmbeddings.sh data/english/embeddings
      time("splitting embeddings", Subprocess("/bin/sh", File(sys.env("HOME"), "workspace/easyccg/splitEmbeddings.sh").getAbsolutePath, embeddingsDir).call(), System.err.println)
    }

    val jobName = File(trainDir).name
    val doTrainingArgs = Vector(File(sys.env("HOME"), "workspace/easyccg/do_training.sh").getAbsolutePath, jobName,
      File(sys.env("HOME"), "workspace/easyccg", embeddingsDir).getAbsolutePath,
      File(sys.env("HOME"), "workspace/easyccg", trainDir).getAbsolutePath,
      File(sys.env("HOME"), "workspace/easyccg", evalDir).getAbsolutePath)
    println(s"cd ${File(sys.env("HOME"), "workspace/easyccg/training")}; ${doTrainingArgs.mkString(" ")}")
    Subprocess("/bin/sh", doTrainingArgs: _*).callWithStreams(System.out, System.err)

    val supertaggerAccuracy = File(embeddingsDir, s"train.${jobName}/log").readLines.toVector.takeRight(2).head.splitWhitespace.last.toDouble
    println(f"Eval set supertagger accuracy: ${supertaggerAccuracy * 100}%.2f")

    val modelDir = File(embeddingsDir, s"model.${jobName}").getAbsolutePath
    makeUnaryRuleFile(trainData, modelDir, lang, minUnaryRuleCount)

    //
    //
    //

    System.err.println("Done training supertagger. Now parsing to get supertag accuracy on output of the tagger.")
    Parse.run(modelDir, inputFileOpt = None,
      evalFileOpt = Some(evalFile.getAbsolutePath),
      outputFileOpt = Some(File(modelDir, s"dev.stagged").getAbsolutePath),
      maxLength = 70,
      numEvalSentences = Int.MaxValue)
    println(f"Eval set supertagger accuracy: ${supertaggerAccuracy * 100}%.2f")
  }

  def makeUnaryRuleFile(trainData: Vector[CcgTree], modelDir: String, lang: String, minUnaryRuleCount: Int) = {
    time("count unary rules", {
      def findUnaryRules(t: CcgTree, i: Int, j: Int): Vector[(Cat, Cat)] = t match {
        case CcgLeaf(cat, word, _) => Vector()
        case CcgUnode(cat, s) => (s.cat.noIndices -> cat.noIndices) +: findUnaryRules(s, i, j)
        case CcgBinode(cat, l, r) => findUnaryRules(l, i, i + l.length) ++ findUnaryRules(r, i + l.length, j)
      }
      val unaryRuleCounts = trainData.flatMap(t => findUnaryRules(t, 0, t.length)).counts.desc // pairs (child, parent)
      for (((child, parent), count) <- unaryRuleCounts) {
        println(f"$count%5d   ${StripCatOuterParens(child)}   ${StripCatOuterParens(parent)}")
      }
      writeUsing(File(modelDir, "unaryRules")) { f =>
        val unaryRules = lang match {
          case "chinese" => unaryRuleCounts.collect { case ((child, parent), count) if count >= minUnaryRuleCount => (child, parent) }
          case "english" => Vector(
            (cat"""N""", cat"""NP"""),
            (cat"""(S[pss]\NP)""", cat"""(N\N)"""),
            (cat"""(S[ng]\NP)""", cat"""(N\N)"""),
            (cat"""(S[adj]\NP)""", cat"""(N\N)"""),
            (cat"""(S[dcl]/NP)""", cat"""(N\N)"""),
            (cat"""(S[to]\NP)""", cat"""(S/S)"""),
            (cat"""(S[pss]\NP)""", cat"""(S/S)"""),
            (cat"""(S[ng]\NP)""", cat"""(S/S)"""),
            (cat"""NP""", cat"""(S[X]/(S[X]\NP))"""),
            (cat"""NP""", cat"""((S[X]\NP)\((S[X]\NP)/NP))"""),
            (cat"""PP""", cat"""((S[X]\NP)\((S[X]\NP)/PP))"""))
        }
        unaryRules.foreach { case (child, parent) => f.wl(s"${StripCatOuterParens(child.noIndices).replace("[conj]", "")} ${StripCatOuterParens(parent.noIndices).replace("[conj]", "")}") }
      }
    }, System.err.println)

    time("count binary rules", {
      def findBinaryRules(t: CcgTree, i: Int, j: Int): Vector[String] = t match {
        case CcgLeaf(cat, word, _) => Vector()
        case CcgUnode(cat, s) => findBinaryRules(s, i, j)
        case CcgBinode(cat, l, r) => (s"${StripCatOuterParens(l.cat.noIndices).replace("[conj]", "")} ${StripCatOuterParens(r.cat.noIndices).replace("[conj]", "")}") +: (findBinaryRules(l, i, i + l.length) ++ findBinaryRules(r, i + l.length, j))
      }
      val binaryRuleCounts = trainData.flatMap(t => findBinaryRules(t, 0, t.length)).counts.desc
      writeUsing(File(modelDir, "seenRules")) { f =>
        val binaryRules = binaryRuleCounts.collect { case (entry, count) if count >= 0 => entry }
        binaryRules.foreach(f.wl)
      }
    }, System.err.println)
  }

  def writeSentences(trees: Vector[CcgTree], writer: BufferedWriter): Unit = {
    trees.map(_.wordposcats).foreach { sentence =>
      writer.wl(sentence.map { case (w, p, c) => f"$w|$p|${StripCatOuterParens(c.noIndices)}" }.mkString(" "))
    }
  }

  class StoppingCountTreeIterator(sub: Iterator[CcgTree], stoppingCount: Int) extends Iterator[CcgTree] {
    private[this] var countSoFar = 0
    def next() = {
      val t = sub.next()
      countSoFar += t.length
      t
    }
    def hasNext = sub.hasNext && (countSoFar <= stoppingCount)
  }

}

