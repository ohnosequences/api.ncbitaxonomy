package ohnosequences.api.ncbitaxonomy.test

import org.scalatest.FunSuite
import ohnosequences.api.ncbitaxonomy._

class ParseNames extends FunSuite {

  val nameLines: Seq[String] =
    Seq(
      """195     |       Campylobacter coli      |               |       scientific name |""",
      """195     |       Campylobacter coli (Doyle 1948) Veron and Chatelain 1973        |               |       authority       |""",
      """195     |       Campylobacter hyoilei   |               |       genbank synonym |""",
      """195     |       Campylobacter hyoilei Alderton et al. 1995      |               |       authority       |""",
      """195     |       DSM 4689        |               |       type material   |""",
      """195     |       JCM 2529        |               |       type material   |""",
      """195     |       LMG 6440        |               |       type material   |""",
      """195     |       LMG 9860        |               |       type material   |""",
      """195     |       NCTC 11366      |               |       type material   |""",
      """195     |       Vibrio coli     |               |       synonym |""",
      """245     |       Flavibacterium aquatile |               |       equivalent name |""",
      """245     |       Flavobacterium aquatile |               |       scientific name |""",
      """245     |       Flavobacterium aquatile (Frankland and Frankland 1889) Bergey et al. 1923 (Approved Lists 1980) emend. Bernardet et al. 1996    |               |       authority       |""",
      """245     |       Flavobacterium aquatile (Frankland and Frankland 1889) Bergey et al. 1923 (Approved Lists 1980) emend. Lee et al. 2012  |               |       authority       |""",
      """245     |       Flavobacterium aquatile (Frankland and Frankland 1889) Bergey et al. 1923 (Approved Lists 1980) emend. Sheu et al. 2013 |               |       authority       |""",
      """245     |       Flavobacterium aquatilis        |               |       synonym |""",
      """245     |       IFO 15052       |               |       type material   |""",
      """245     |       JCM 20475       |               |       type material   |"""
    )

  test("only scientific names are parsed") {

    assert {
      dmp.names.fromLines(nameLines.toIterator).toSeq === Seq(
        ScientificName(195, "Campylobacter coli"),
        ScientificName(245, "Flavobacterium aquatile")
      )
    }
  }
}
