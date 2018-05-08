package ohnosequences.api.ncbitaxonomy

sealed abstract class Rank
case object Rank {

  case object Superkingdom    extends Rank
  case object Kingdom         extends Rank
  case object Subkingdom      extends Rank
  case object Superphylum     extends Rank
  case object Phylum          extends Rank
  case object Subphylum       extends Rank
  case object Superclass      extends Rank
  case object Class           extends Rank
  case object Subclass        extends Rank
  case object Infraclass      extends Rank
  case object Cohort          extends Rank
  case object Superorder      extends Rank
  case object Order           extends Rank
  case object Suborder        extends Rank
  case object Infraorder      extends Rank
  case object Parvorder       extends Rank
  case object Superfamily     extends Rank
  case object Family          extends Rank
  case object Subfamily       extends Rank
  case object Tribe           extends Rank
  case object Subtribe        extends Rank
  case object Genus           extends Rank
  case object Subgenus        extends Rank
  case object SpeciesGroup    extends Rank
  case object SpeciesSubgroup extends Rank
  case object Species         extends Rank
  case object Subspecies      extends Rank
  case object Varietas        extends Rank
  case object Forma           extends Rank
  case object NoRank          extends Rank

  def apply(str: String): Rank =
    // Normalize str: remove all whitespaces, make it lower case
    str.replaceAll("\\s", "").toLowerCase match {
      case "superkingdom"    => Superkingdom
      case "kingdom"         => Kingdom
      case "subkingdom"      => Subkingdom
      case "superphylum"     => Superphylum
      case "phylum"          => Phylum
      case "subphylum"       => Subphylum
      case "superclass"      => Superclass
      case "class"           => Class
      case "subclass"        => Subclass
      case "infraclass"      => Infraclass
      case "cohort"          => Cohort
      case "superorder"      => Superorder
      case "order"           => Order
      case "suborder"        => Suborder
      case "infraorder"      => Infraorder
      case "parvorder"       => Parvorder
      case "superfamily"     => Superfamily
      case "family"          => Family
      case "subfamily"       => Subfamily
      case "tribe"           => Tribe
      case "subtribe"        => Subtribe
      case "genus"           => Genus
      case "subgenus"        => Subgenus
      case "speciesgroup"    => SpeciesGroup
      case "speciessubgroup" => SpeciesSubgroup
      case "species"         => Species
      case "subspecies"      => Subspecies
      case "varietas"        => Varietas
      case "forma"           => Forma
      case "norank"          => NoRank
    }
}
