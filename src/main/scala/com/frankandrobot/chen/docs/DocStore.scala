package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{Doc, DocId, RawDoc, RawTerm}
import com.frankandrobot.chen.indexer.Indexer


class DocStore(indexer : Indexer) {

  private var _docs = List[RawDoc]()

  def docs() = _docs

  def add(id : String, title : String, doc : String) = {

    _docs = RawDoc(DocId(id), title, doc) :: _docs
  }

  def extractDocRawTerms() = _docs.map(index)

  def index(doc : RawDoc) = Doc(doc.toDocLite, indexer.index(doc.title + ". " +doc.contents).map(RawTerm(_)))
}
