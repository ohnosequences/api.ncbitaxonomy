package ohnosequences.api.ncbitaxonomy

trait AnyNode extends Any {

  def ID: TaxID
  def parentID: Option[TaxID]
  def rank: String
  // there's more data there
}

sealed trait AnyNodeName {

  def nodeID: TaxID
  def name: String
}

final case class ScientificName(nodeID: TaxID, name: String) extends AnyNodeName
