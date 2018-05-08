package ohnosequences.api.ncbitaxonomy.test

import ohnosequences.api.ncbitaxonomy._
import ohnosequences.api.ncbitaxonomy.test.utils._
import ohnosequences.db
import ohnosequences.awstools.s3.S3Object
import org.scalatest.FunSuite
import java.io.File

class FullTaxonomySpec extends FunSuite {

  object data {
    val rootID: TaxID = 1

    object nodes {

      val localFile =
        new File(s"./data/in/${db.ncbitaxonomy.version}/nodes.dmp")

      def lines = getLines(db.ncbitaxonomy.nodes, localFile)

      val map: TreeMap = io.generateNodesMap(lines)
    }

    object names {
      val localFile =
        new File(s"./data/in/${db.ncbitaxonomy.version}/names.dmp")

      def lines = getLines(db.ncbitaxonomy.names, localFile)

      val map: Map[TaxID, String] = io.generateNamesMap(lines)
    }
  }

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

  def printToFile(f: java.io.File)(op: java.io.PrintWriter => Unit): Unit = {
    val p = new java.io.PrintWriter(f)
    try { op(p) } finally { p.close() }
  }
}
