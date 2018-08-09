package ohnosequences.api

import ohnosequences.trees.{Node, Tree}

package object ncbitaxonomy {

  private[ncbitaxonomy] type +[A, B] =
    Either[A, B]

  type TaxID =
    Int

  type TaxNode = Node[TaxID]
  type TaxTree = Tree[TaxID]

  type TreeMap = Map[TaxID, (Option[TaxID], Array[TaxID])]

  def toTaxID: String => TaxID =
    _.toInt

  def TreeMap(): TreeMap = Map[TaxID, (Option[TaxID], Array[TaxID])]()
}
