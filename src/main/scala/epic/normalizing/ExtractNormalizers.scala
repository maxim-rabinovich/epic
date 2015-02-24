package epic.normalizing

import breeze.config.CommandLineParser
import epic.trees.{AnnotatedLabel, TreeInstance, ProcessedTreebank}

/**
 * Created by mrabinovich on 2/23/2015.
 */
object ExtractNormalizers extends App {

  def sentenceIsTooLong(p: TreeInstance[AnnotatedLabel, String], maxLength: Int): Boolean = {
    p.words.count(x => x == "'s" || x(0).isLetterOrDigit) > maxLength
  }

  val parser = epic.parser.models.en.span.EnglishSpanParser.load()

  val tb = CommandLineParser.readIn[ProcessedTreebank](args)
  val trainTrees = tb.trainTrees
  val theTrees = trainTrees.toIndexedSeq.filterNot(sentenceIsTooLong(_, 60))

  val Zs : Array[Double] = new Array[Double](theTrees.length)
  var ctr : Int = 0
  for (tree <- theTrees) {
    val words = tree.words
    Zs(ctr) = parser.marginal(words).logPartition

    ctr += 1
  }

  println("=====PRINTING FINAL NORMALIZERS=====")
  for (i <- 0 until Zs.length) {
    printf("%d %g\n", i, Zs(i))
  }
  println("====================================")
}
