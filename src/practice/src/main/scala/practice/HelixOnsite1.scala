package practice

object HelixOnSite1 extends App {

  case class GeneSeq(pos: Int, base: Char)

  case class FilterRange(start: Int, end: Int) {
    def isInRange(g: GeneSeq): Boolean = {
      g.pos >= start && g.pos < end
    }

    def isBehind(g: GeneSeq): Boolean = {
      g.pos >= end
    }
  }

  def filterGenesNaive(
    genes: Seq[GeneSeq],
    filterRanges: Seq[FilterRange]): Seq[GeneSeq] = {

    for {
      g <- genes
      r <- filterRanges
      if r.isInRange(g)
    } yield g
  }

  def filterGenes(
    genes: Seq[GeneSeq],
    filterRanges: Seq[FilterRange]): Seq[GeneSeq] = {

    var g, f = 0
    var out = Seq[GeneSeq]()

    while (g < genes.length && f < filterRanges.length) {
      def filterIsInRange = filterRanges(f).isInRange(genes(g))
      def filterIsBehind = filterRanges(f).isBehind(genes(g))

      if (filterIsInRange) {
        out = out :+ genes(g)
        g = g + 1
      } else if (filterIsBehind) {
        f = f + 1
      } else {
        g = g + 1
      }
    }
    out
  }

  type Aggr = (Seq[GeneSeq], Seq[FilterRange])

  def filterGenesWithFold(
    genes: Seq[GeneSeq],
    filterRanges: Seq[FilterRange]): Seq[GeneSeq] = {

    def append(aggr: Aggr, g: GeneSeq): Aggr = {
      val (output, remainingFilter) = aggr
      if (remainingFilter.isEmpty) {
        aggr
      } else if (remainingFilter.head.isInRange(g)) {
        (output :+ g, remainingFilter)
      } else if (remainingFilter.head.isBehind(g)) {
        (output, remainingFilter.tail)
      } else {
        aggr
      }
    }
    val initAggr = (Seq[GeneSeq](), filterRanges)
    val (output, _) = genes.foldLeft(initAggr)(append)
    output
  }

  // test data
  val genes1 = List(GeneSeq(1, 'A'), GeneSeq(2, 'C'))
  val ranges1 = List(FilterRange(1, 2))

  val genes2 = List(GeneSeq(5, 'G'), GeneSeq(6, 'A'), GeneSeq(7, 'T'),
    GeneSeq(15, 'G'), GeneSeq(19, 'T'), GeneSeq(20, 'C'), GeneSeq(22, 'T'))
  val ranges2 = List(FilterRange(6, 7), FilterRange(15, 20))

  println(filterGenesNaive(genes1, ranges1))
  println(filterGenesNaive(genes2, ranges2))

  println(filterGenes(genes1, ranges1))
  println(filterGenes(genes2, ranges2))

  println(filterGenesWithFold(genes1, ranges1))
  println(filterGenesWithFold(genes2, ranges2))

}