package practice

object HelixOnSite2 extends App {

  case class Codon(bases: String) {
    require(bases.length() == 3)
  }

  object Protein extends Enumeration {
    val Met, Phe, Val, Tyr, Stop, Arg, Lys = Value
  }

  val codonMap = Map(
    Codon("AUG") -> Protein.Met,
    Codon("UUU") -> Protein.Phe,
    Codon("GUA") -> Protein.Val,
    Codon("UAU") -> Protein.Tyr,
    Codon("UAA") -> Protein.Stop,
    Codon("UAG") -> Protein.Stop,
    Codon("UGA") -> Protein.Stop,
    Codon("CGU") -> Protein.Arg,
    Codon("AAG") -> Protein.Lys)

  val validBases = Set('A', 'C', 'T', 'G')
  val startCodon = Codon("AUG")
  val stopCodons = Set(Codon("UAA"), Codon("UAG"), Codon("UGA"))

  def validate(dna: String): Boolean = {
    dna.forall(_.isUpper) && dna.forall(validBases.contains) && (dna.size % 3 == 0)
  }

  def toMRna(dna: String): String = {
    dna.map(base => if (base == 'T') 'U' else base)
  }

  def toCodons(mRna: String): Seq[Codon] = {
    mRna.sliding(3, 3).toList.map(Codon(_))
  }

  def translate(codons: Seq[Codon]): Seq[Codon] = {
    // find start codon then parse until stop codon
    codons
      .dropWhile(codon => codon != startCodon)
      .takeWhile(codon => !stopCodons.contains(codon))
  }

  def synthesize(dna: String): Seq[Protein.Value] = {
    if (!validate(dna)) {
      Seq()
    } else {
      val mRNA = toMRna(dna) // map to mRNA
      val codons = toCodons(mRNA) // build codons
      val translated = translate(codons) // translate
      translated.map(codon => codonMap(codon)) // lookup codon -> amino acid
    }
  }

  println(synthesize("ATGTTTGTATATTAG"))
}