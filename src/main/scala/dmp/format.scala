package ohnosequences.api.ncbitaxonomy.dmp

import ohnosequences.api.ncbitaxonomy._
import scala.collection.immutable.Queue
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
  type TreeMap = Map[TaxID, (Option[TaxID], Array[TaxID])]
  def TreeMap(): TreeMap = Map[TaxID, (Option[TaxID], Array[TaxID])]()

  val sep    = ","
  val sepAux = ";"

  // Return a map TaxID -> (Option[ParentID], List[ChildID])
  @SuppressWarnings(Array("org.wartremover.warts.MutableDataStructures"))
  def generateNodesMap(lines: Iterator[String]): TreeMap = {
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
    val wholeMap: TreeMap = parentsMap
      .foldLeft(TreeMap()) {
        case (currentMap, (nodeID, parentID)) =>
          val children =
            childrenMap.get(nodeID).map({ _.toArray }).getOrElse(Array[TaxID]())
          val value = (parentID, children)
          currentMap + (nodeID -> value)
      }

    // Return whole map
    wholeMap
  }

  def treeToIterators(map: TreeMap,
                      root: TaxID): (Iterator[String], Iterator[String]) = {
    @annotation.tailrec
    def treeToFile_rec(
        unvisitedNodes: Queue[TaxID],
        in: Iterator[String],
        out: Iterator[String]
    ): (Iterator[String], Iterator[String]) = {
      val (parent, poppedQueue) = unvisitedNodes.dequeue
      val children              = map(parent)._2

      // Parent_ID,[Child1_ID[;Child2_ID[; Child3_ID[...]]]]
      val childrenLine = Array(
        parent.toString,
        children.mkString(sepAux)
      ).mkString(sep)

      // Child_ID,[ParentID]
      val parentLines: Array[String] = children map { child =>
        Array(child.toString, parent.toString).mkString(sep)
      }

      val nextQueue = poppedQueue ++ children

      if (nextQueue.isEmpty)
        (in ++ Array(childrenLine), out ++ parentLines)
      else
        treeToFile_rec(nextQueue, in ++ Array(childrenLine), out ++ parentLines)
    }

    treeToFile_rec(
      Queue(root),
      Iterator[String](),
      Iterator[String](s"$root$sep")
    )
  }

  def treeFromIterators(in: Iterator[String],
                        out: Iterator[String]): TreeMap = {
    val childrenMap = in.foldLeft(Map[TaxID, Array[TaxID]]()) {
      case (currentMap, currentString) =>
        val srcDst        = currentString.split(sep).map(_.trim)
        val parent: TaxID = srcDst(0).toInt
        val children: Array[TaxID] =
          srcDst
            .lift(1)
            .fold(Array[TaxID]()) { arr =>
              arr.split(sepAux).map(_.trim.toInt)
            }

        currentMap + (parent -> children)
    }

    out.foldLeft(TreeMap()) {
      case (currentMap, currentString) =>
        val srcDst                = currentString.split(sep).map(_.trim)
        val child: TaxID          = srcDst(0).toInt
        val parent: Option[TaxID] = srcDst.lift(1).map(_.toInt)

        currentMap + (child -> ((parent, childrenMap(child))))
    }
  }
}
