package com.frankandrobot.chen


package object DocTypes {

  /**
    * Can be anything that uniquely identifies it, like a URL
    * @param id
    */
  case class DocId(val id : String)

  case class Doc(val docId : DocId,
                 val title : String,
                 val contents : String) {

    def toDocLite() = { DocLite(docId, title, contents.substring(0, 140)) }
  }

  case class DocLite(val docId : DocId, val title : String, val blurb : String)

  class RawTerm(val value : String)

  object RawTerm {
    def apply(value : String) = new RawTerm(value)
  }

  /**
    * When you put these in a list, you get the raw terms grouped by doc
    *
    * @param doc
    * @param terms
    */
  case class DocWithRawTerms(val doc : DocLite,
                             val terms: List[RawTerm])

  case class Term(override val value : String, val docs : Set[DocLite]) extends RawTerm(value)
}
