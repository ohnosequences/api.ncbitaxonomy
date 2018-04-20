package ohnosequences.api.ncbitaxonomy.test

import org.scalatest.FunSuite
import org.scalatest.OptionValues.convertOptionToValuable
import ohnosequences.api.ncbitaxonomy._

class ParseNodes extends FunSuite {

  val nodeLines: Seq[String] =
    Seq(
      """318     |       29438   |       no rank |               |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """319     |       29438   |       no rank |               |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """321     |       317     |       no rank |               |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """322     |       47877   |       no rank |               |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """323     |       251701  |       no rank |               |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """329     |       48736   |       species |       RP      |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |""",
      """330     |       1232139 |       species |       PP      |       0       |       1       |       11      |       1       |       0       |       1       |       1       |       0       |               |"""
    )

  test("parse several nodes") {

    val nodes =
      dmp.nodes.fromLines(nodeLines.toIterator).toSeq

    val firstNode = nodes.headOption.value
    val lastNode  = nodes.lastOption.value

    assert {
      (firstNode.ID === 318) &&
      (firstNode.parentID === Some(29438)) &&
      (firstNode.rank === "no rank")
    }

    assert {
      (lastNode.ID === 330) &&
      (lastNode.parentID === Some(1232139)) &&
      (lastNode.rank === "species")
    }
  }
}
