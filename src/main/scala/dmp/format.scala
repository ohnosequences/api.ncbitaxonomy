package ohnosequences.api.ncbitaxonomy.dmp

import ohnosequences.api.ncbitaxonomy._
import scala.collection.mutable.{
  ArrayBuffer => MutableArrayBuffer,
  Map => MutableMap
}

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

case object parse {
  // Return a map TaxID -> (Option[ParentID], List[ChildID])
  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  def generateNodesMap(
      lines: Iterator[String]
  ): Map[TaxID, (Option[TaxID], Array[TaxID])] = {
    // Create a map TaxID -> Option[ParentID]
    val parentsMap: Map[TaxID, Option[TaxID]] =
      dmp.nodes
        .fromLines(lines)
        .map({ node =>
          node.ID -> node.parentID
        })
        .toMap

    // Create a map TaxID -> List[ChildID]
    val childrenMap: MutableMap[TaxID, MutableArrayBuffer[TaxID]] =
      parentsMap.foldLeft(MutableMap[TaxID, MutableArrayBuffer[TaxID]]()) {
        case (currentMap, (nodeID, optParentID)) =>
          optParentID.fold(
            currentMap
          ) { parentID =>
            if (currentMap.isDefinedAt(parentID)) {
              currentMap(parentID) += nodeID
            } else {
              currentMap += (parentID -> MutableArrayBuffer(nodeID))
            }
            currentMap
          }
      }

    // Create a map TaxID -> (Option[ParentID], List[ChildID])
    val wholeMap: Map[TaxID, (Option[TaxID], Array[TaxID])] = parentsMap
      .foldLeft(Map[TaxID, (Option[TaxID], Array[TaxID])]()) {
        case (currentMap, (nodeID, parentID)) =>
          val children =
            childrenMap.get(nodeID).map({ _.toArray }).getOrElse(Array[TaxID]())
          val value = (parentID, children)
          currentMap + (nodeID -> value)
      }

    // Return whole map
    wholeMap
  }
}
