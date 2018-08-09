package ohnosequences.api.ncbitaxonomy

import ohnosequences.trees.{Node, NodePosition}
import scala.collection.immutable.Queue
import scala.collection.mutable.{
  ArrayBuffer => MutableArrayBuffer,
  Map => MutableMap
}

case object io {
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

  def generateNamesMap(lines: Iterator[String]): Map[TaxID, String] =
    dmp.names
      .fromLines(lines)
      .map({ name =>
        name.nodeID -> name.name
      })
      .toMap

  def namesToIterators(
      namesMap: Map[TaxID, String]
  ): (Iterator[String], Iterator[String]) = {
    val itIn = namesMap.map({ case (k, v) => s"$k$sep$v" }).toIterator
    val itOut = namesMap
      .groupBy(_._2)
      .map({
        case (k, v) =>
          s"$k$sep${v.keys.mkString(sepAux)}"
      })
      .toIterator

    (itIn, itOut)
  }

  def namesFromIterator(itIn: Iterator[String]): Map[TaxID, String] =
    itIn
      .map({ str =>
        // We cannot use split here, as the name may contain the separator
        // used; instead, we find the first separator (the ID is numeric and
        // does not contain it) and split there using take and drop.
        val sepIdx = str.indexOf(sep)
        val id     = toTaxID(str take sepIdx)
        val name   = str drop (sepIdx + 1)

        id -> name
      })
      .toMap

  def generateRanksMap(lines: Iterator[String]): Map[TaxID, Rank] =
    dmp.nodes
      .fromLines(lines)
      .map({ node =>
        node.ID -> Rank(node.rank)
      })
      .toMap

  def ranksToIterators(
      ranksMap: Map[TaxID, Rank]
  ): (Iterator[String], Iterator[String]) = {
    val itIn = ranksMap.map({ case (k, v) => s"$k$sep$v" }).toIterator
    val itOut = ranksMap
      .groupBy(_._2)
      .map({
        case (k, v) =>
          s"$k$sep${v.keys.mkString(sepAux)}"
      })
      .toIterator

    (itIn, itOut)
  }

  def ranksFromIterator(itIn: Iterator[String]): Map[TaxID, Rank] =
    itIn
      .map({ str =>
        val srcDst = str.split(sep).map(_.trim)
        val id     = toTaxID(srcDst(0))
        val rank   = Rank(srcDst(1))

        id -> rank
      })
      .toMap

  def treeMapToTaxTree(wholeMap: TreeMap, rootID: TaxID): TaxTree = {
    val rootChildren = wholeMap(rootID)._2
    val root: TaxNode =
      new TaxNode(rootID, None, 0, rootChildren.length)

    val rootLevel: Array[TaxNode]            = Array(root)
    val initialLevels: Array[Array[TaxNode]] = Array(rootLevel)

    @SuppressWarnings(
      Array("org.wartremover.warts.Var", "org.wartremover.warts.While")
    )
    @annotation.tailrec
    def generateTaxTree_rec(
        levels: Array[Array[TaxNode]]
    ): Array[Array[TaxNode]] = {
      val lastLevel = levels.last

      // Compute next level length
      var length = 0
      lastLevel foreach { parent =>
        val (_, childrenIDs) = wholeMap(parent.payload)
        length += childrenIDs.length
      }

      if (length == 0) {
        levels
      } else {
        // Create empty array for next level
        val nextLevel = new Array[TaxNode](length)

        // Populate nextLevel
        var childrenOffset      = 0
        var grandChildrenOffset = 0
        var parentPos           = 0

        // Iterate nodes in current level and add their children to nextLevel
        while (parentPos < lastLevel.length) {
          val parent           = lastLevel(parentPos)
          val (_, childrenIDs) = wholeMap(parent.payload)

          var i = 0
          while (i < childrenIDs.length) {
            val childID               = childrenIDs(i)
            val (_, grandChildrenIDs) = wholeMap(childID)
            val grandChildrenNum      = grandChildrenIDs.length

            nextLevel(childrenOffset + i) = new Node(childID,
                                                     Some(parentPos),
                                                     grandChildrenOffset,
                                                     grandChildrenNum)

            i += 1
            grandChildrenOffset += grandChildrenNum
          }

          childrenOffset += childrenIDs.length
          parentPos += 1
        }

        generateTaxTree_rec(levels :+ nextLevel)
      }
    }

    new TaxTree(generateTaxTree_rec(initialLevels))
  }

  def taxTreeToTreeMap(taxTree: TaxTree): TreeMap =
    taxTree.toIterator.foldLeft(TreeMap()) {
      case (map, nodePos) =>
        val node = taxTree(nodePos).payload
        val parent = taxTree.parent(nodePos) map { parentPos =>
          taxTree(parentPos).payload
        }
        val children = taxTree.children(nodePos) map { childPos =>
          taxTree(childPos).payload
        }

        map + (node -> ((parent, children)))
    }

  def taxSubTreeIndicesToTreeMap(
      taxTree: TaxTree,
      positions: Array[NodePosition]
  ): TreeMap =
    positions.foldLeft(TreeMap()) {
      case (map, nodePos) =>
        val node = taxTree(nodePos).payload
        val parent = taxTree.parent(nodePos) map { parentPos =>
          taxTree(parentPos).payload
        }
        val children = taxTree.children(nodePos) map { childPos =>
          taxTree(childPos).payload
        }

        map + (node -> ((parent, children)))
    }
}
