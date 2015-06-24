package dhg.easyccg

import dhg.util._
import uk.ac.ed.easyccg.main.EasyCCG

/**
 * @author dhg
 */
object Parse {

  def main(args: Array[String]): Unit = {

    val (arguments, options) = parseArgs(args)

    val modelDir = options("model") // training/chinese-embeddings/model.chinese-250k
    val inputFile = options("inputFile")
    val outputFile = options("outputFile")

    // cd ~/bin/easyccg/; java -jar easyccg.jar --model training/chinese-embeddings/model.chinese-250k 
    //                                          --inputFile chinese-dev-tokenized.txt  
    //                                          --inputFormat tokenized  
    //                                          --outputFormat supertags  
    //                                          > training/chinese-embeddings/model.chinese-250k/chinese-dev-tokenized.out

    // cd ~/bin/easyccg/; java -jar easyccg.jar --model data/chinese/chinese-embeddings/model.chinese-250k 
    //                                          --inputFile data/chinese/chinese-dev-tokenized.txt  
    //                                          --inputFormat tokenized  
    //                                          --outputFormat supertags  
    //                                          --outputFile data/chinese/chinese-250k/chinese-dev-tagged.out

    EasyCCG.main(Array(
      "--model", modelDir,
      "--inputFile", inputFile,
      "--inputFormat", "tokenized",
      "--outputFormat", "supertags",
      "--outputFile", outputFile))

  }

}

