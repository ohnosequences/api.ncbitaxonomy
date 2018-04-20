package ohnosequences.api

package object ncbitaxonomy {

  private[ncbitaxonomy] type +[A, B] =
    Either[A, B]

  type TaxID =
    Int

  def toTaxID: String => TaxID =
    _.toInt

}
