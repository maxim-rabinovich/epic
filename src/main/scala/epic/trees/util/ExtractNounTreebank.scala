package epic.trees.util

import java.io._

import epic.trees._

object ExtractNounTreebank {
  def main(argv : Array[String]) : Unit = {
    val ptbPath : String = argv(0)
    val ptbFile : File   = new File(ptbPath)

    val reader  : PennTreeReader =
      new PennTreeReader(new BufferedReader(new FileReader(ptbFile)))

    val NPTreebank : List[(Tree[String], IndexedSeq[String])] = {
      for ((tree, sent) <- reader)
        yield extractNP(tree, sent)
    }.flatMap(x => x).toList

    for (counter <- 0 until 10) {
      println("NP Tree %d:\n%s\n"
                  .format(counter, NPTreebank(counter)._1.render(NPTreebank(counter)._2)))
    }

    return
  }

  private def extractNP(tree : Tree[String], sent : IndexedSeq[String])
  : List[(Tree[String], IndexedSeq[String])] = {
    if (tree.isLeaf && !tree.label.startsWith("NN")) {
      return List.empty
    }

    if (tree.label.startsWith("NP")) {
      return List((tree, sent.slice(tree.span.begin, tree.span.end)))
    }

    val childLists = for (child <- tree.children.toList)
      yield extractNP(child, sent)

    childLists.flatMap(x => x)
  }
}
