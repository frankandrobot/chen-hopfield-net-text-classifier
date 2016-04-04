package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{Doc, DocId, DocWithRawTerms, RawTerm}
import com.frankandrobot.chen.indexer.Indexer


class DocStore(indexer : Indexer) {

  private var _docs = List[Doc]()

  def docs() = _docs

  def add(id : String, title : String, doc : String) = {

    _docs = Doc(DocId(id), title, doc) :: _docs
  }

  def extractDocRawTerms() = _docs.map(index)

  def index(doc : Doc) = DocWithRawTerms(doc.toDocLite, indexer.index(doc.title + ". " +doc.contents).map(RawTerm(_)))
}
