package ohnosequences.api.ncbitaxonomy.dmp

import ohnosequences.api.ncbitaxonomy._

case object row {

  val fieldSeparator: Char = '|'
  val endOfRow: String     = "|"

  def fromLine(line: String): Array[String] =
    line
      .stripSuffix(endOfRow)
      .split(fieldSeparator)
      .map(_.trim)
      .toArray[String]
}

class Node(val fields: Array[String]) extends AnyNode {

  def ID: TaxID =
    toTaxID(fields(0))

  def parentID: Option[TaxID] = {
    val parent = toTaxID(fields(1))
    if (parent == ID)
      None
    else
      Some(parent)
  }

  def rank: String =
    fields(2)
}

case object Node {

  def from(line: String): Node =
    new Node(row.fromLine(line))
}

case object nodes {

  def fromLines(lines: Iterator[String]): Iterator[Node] =
    lines map Node.from
}

case object names {

  def fromLines(lines: Iterator[String]): Iterator[ScientificName] =
    lines
      .collect {
        case line if (row.fromLine(line)(3) == "scientific name") =>
          val r = row.fromLine(line)
          ScientificName(toTaxID(r(0)), r(1))
      }
}
