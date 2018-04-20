package ohnosequences.api.ncbitaxonomy.test

import org.scalatest.FunSuite
import ohnosequences.api.ncbitaxonomy._
import ohnosequences.db
import ohnosequences.api.ncbitaxonomy.test.utils._
import ohnosequences.test.ReleaseOnlyTest
import ohnosequences.awstools.s3.S3Object
import java.io.File

class ParseFullTaxonomy extends FunSuite {

  /**
    * Auxiliary method that returns an Iterator[String] from `file`. If `file`
    * does not exist, it is downloaded from `s3Object` before parsing its lines.
    */
  def getLines(s3Object: S3Object, file: File): Iterator[String] = {
    if (!file.exists)
      downloadFrom(s3Object, file).left
        .map { e =>
          fail(e.msg)
        }

    retrieveLinesFrom(file) match {
      case Right(x) => x
      case Left(e)  => fail(e.msg)
    }
  }

  def getNamesLines = getLines(db.ncbitaxonomy.names, data.namesLocalFile)
  def getNodesLines = getLines(db.ncbitaxonomy.nodes, data.nodesLocalFile)

  test("Parse all names and access all data", ReleaseOnlyTest) {

    dmp.names.fromLines(getNamesLines) foreach { n =>
      val id   = n.nodeID
      val name = n.name

      // We just want to check whether we can access the values but sbt
      // complaints about the values above being unused, so trick sbt into
      // thinkink we are using them.
      // TODO: Code a proper test instead of this silly trick.
      (id, name)
    }
  }

  test("Parse all nodes and access all data", ReleaseOnlyTest) {

    dmp.nodes.fromLines(getNodesLines) foreach { node =>
      val id     = node.ID
      val parent = node.parentID
      val rank   = node.rank

      // We just want to check whether we can access the values but sbt
      // complaints about the values above being unused, so trick sbt into
      // thinkink we are using them.
      // TODO: Code a proper test instead of this silly trick.
      (id, parent, rank)
    }
  }
}
