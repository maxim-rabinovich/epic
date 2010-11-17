package scalanlp.parser
package projections

import scalala.tensor.counters.Counters.DoubleCounter
import scalanlp.config.Configuration
import scalanlp.trees._

import ChartParser._;
import CoarseToFineParser._;

class CoarseToFineParser[Chart[X]<:ParseChart[X],C,F,W](coarseParser: ChartParser[Chart,C,W],
                                proj: F=>C,
                                val root: F,
                                val lexicon: Lexicon[F,W],
                                val grammar: Grammar[F],
                                chartFactory: ParseChart.Factory[Chart] = ParseChart.viterbi,
                                threshold:Double = -10) extends ChartParser[Chart,F,W] {

  val indexedProjections = new ProjectionIndexer(coarseParser.grammar.index, grammar.index, proj);

  private val coarseRootIndex = coarseParser.grammar.index(proj(root));

  private val fineParser = new CKYParser[Chart,F,W](root,lexicon,grammar,chartFactory);

  def buildInsideChart(s: Seq[W], validSpan: SpanScorer = defaultScorer):Chart[F] = {
    val chartScorer = coarseSpanScorerFromParser(s, coarseParser, indexedProjections, threshold);
    val myScorer = SpanScorer.sum(chartScorer,validSpan);
    fineParser.buildInsideChart(s, myScorer);
  }


  /**
   * Given an inside chart, fills the passed-in outside parse chart with inside scores.
   */
  def buildOutsideChart(inside: ParseChart[F],
                        validSpan: SpanScorer = defaultScorer):Chart[F] = {
    fineParser.buildOutsideChart(inside, validSpan);
  }

  def withCharts[Chart[X]<:ParseChart[X]](factory: ParseChart.Factory[Chart]) = {
    val cc = coarseParser.withCharts(factory);
    new CoarseToFineParser[Chart,C,F,W](cc,proj, root,lexicon,grammar,factory, threshold);
  }
}


object CoarseToFineParser {
  def coarseChartSpanScorer[C](proj: Int=>Int,
                               coarseInside: ParseChart[C],
                               coarseOutside:ParseChart[C],
                               sentProb:Double,
                               threshold:Double = -10):SpanScorer = new SpanScorer {

    @inline
    def score(begin: Int, end: Int, label: Int) = {
      val score =  (coarseInside(begin,end,proj(label))
              + coarseOutside(begin,end,proj(label)) - sentProb);
      if (score > threshold) 0.0 else Double.NegativeInfinity;
    }

    def scoreUnaryRule(begin: Int, end: Int, parent: Int, child: Int) = score(begin,end,parent);


    def scoreBinaryRule(begin: Int, split: Int, end: Int, parent: Int, leftChild: Int, rightChild: Int) = {
      score(begin,end,parent);
    }
  }

  def coarseSpanScorerFromParser[W,L,Chart[X]<:ParseChart[X]](s: Seq[W],
                                                              coarseParser: ChartParser[Chart,L,W],
                                                              proj: Int=>Int,
                                                              threshold: Double = -10) = {
    val coarseRootIndex = coarseParser.grammar.index(coarseParser.root);
    val coarseInside = coarseParser.buildInsideChart(s)
    val coarseOutside = coarseParser.buildOutsideChart(coarseInside);

    val sentProb = coarseInside(0,s.length,coarseRootIndex);
    assert(!sentProb.isInfinite, s);

    val chartScorer = CoarseToFineParser.coarseChartSpanScorer(proj,
      coarseInside, coarseOutside, sentProb, threshold);

    chartScorer
  }
}

object SimpleCoarseToFineTrainer extends ParserTrainer {
  def trainParser(trainTrees: Seq[(BinarizedTree[String],Seq[String])],
                  devTrees: Seq[(BinarizedTree[String],Seq[String])],
                  config: Configuration) = {
    def proj(label: String) =  if(label == "" ) label else "X";
    val coarseTrees = for {
      (tree,words) <- trainTrees
    } yield (tree map proj, words);

    val coarse = GenerativeParser.fromTrees(coarseTrees);

    val (fineLexicon,fineGrammar) = GenerativeParser.extractLexiconAndGrammar(trainTrees.iterator);
    val fine = new CoarseToFineParser[ParseChart.ViterbiParseChart,String,String,String](coarse, proj _, "", fineLexicon, fineGrammar, ParseChart.viterbi);

    Iterator.single(("CtF", fine));
  }
}
