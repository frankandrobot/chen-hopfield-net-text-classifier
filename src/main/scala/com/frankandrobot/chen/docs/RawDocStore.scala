package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{Doc, DocId, RawDoc, RawTerm}
import com.frankandrobot.chen.indexer.Indexer
import com.frankandrobot.chen.utils.Parallel

import scala.concurrent.Await
import scala.concurrent.duration.Duration


class RawDocStore(indexer : Indexer) {

  private var _docs = Vector[RawDoc]()

  def docs() = _docs

  def add(rawDoc: RawDoc) = _docs = rawDoc +: _docs

  def add(id : String, title : String, doc : String) = {

    _docs = RawDoc(DocId(id), title, doc) +: _docs
  }

  def toDocs() = Await.result(Parallel.map(_docs, _index), Duration.Inf)

  private def _index(doc : RawDoc) = {

    val tokens = new Indexer().indexConsecutiveTerms(doc.title + ". " + doc.contents).map(RawTerm(_))

    Doc(doc.toDocLite, tokens, _histogram(tokens))
  }

  /**
    * We use a mutable structure because when dealing with 1000s of terms, it's faster
    *
    */
  private def _histogram(terms : List[RawTerm]): collection.Map[String, Int] = {

    val hash = collection.mutable.HashMap.empty[String, Int] withDefaultValue 0
    terms foreach { cur => hash(cur.value) += 1 }
    hash
  }
}