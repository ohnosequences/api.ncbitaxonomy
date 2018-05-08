package ohnosequences.api.ncbitaxonomy.test

import ohnosequences.api.ncbitaxonomy._
import ohnosequences.test.ReleaseOnlyTest

class ParseFullTaxonomy extends FullTaxonomySpec {

  test("Parse all names and access all data", ReleaseOnlyTest) {

    dmp.names.fromLines(data.names.lines) foreach { n =>
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

    dmp.nodes.fromLines(data.nodes.lines) foreach { node =>
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

class IO extends FullTaxonomySpec {

  test("treeFromIterators ∘ treeToIterators ≡ identity", ReleaseOnlyTest) {
    val (itIn, itOut)  = io.treeToIterators(data.nodes.map, data.rootID)
    val parsedNodesMap = io.treeFromIterators(itIn, itOut)

    // Check parsedNodesMap == data.nodes.map, but tuples and arrays equality
    // make everything a little bit more cumbersome
    assert { parsedNodesMap.keySet == data.nodes.map.keySet }
    parsedNodesMap foreach {
      case (k, parsedValue) =>
        val value = data.nodes.map(k)
        assert { parsedValue._1 == value._1 }
        assert { parsedValue._2 sameElements value._2 }
    }
  }

  test("namesFromIterators ∘ namesToIterators ≡ identity", ReleaseOnlyTest) {
    val (itIn, itOut)  = io.namesToIterators(data.names.map)
    val parsedNamesMap = io.namesFromIterator(itIn)

    parsedNamesMap == data.names.map
  }
}
