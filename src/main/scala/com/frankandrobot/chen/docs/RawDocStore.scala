package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{Doc, DocId, RawDoc, RawTerm}
import com.frankandrobot.chen.indexer.Indexer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}


class RawDocStore(indexer : Indexer) {

  private var _docs = Vector[RawDoc]()

  def docs() = _docs

  def add(id : String, title : String, doc : String) = {

    _docs = RawDoc(DocId(id), title, doc) +: _docs
  }

  def toDocs() = Await.result(Helper._concurrentActionOnSeq(_docs, _index), Duration.Inf)

  private def _index(doc : RawDoc) = {

    val tokens = new Indexer().index(doc.title + ". " + doc.contents).map(RawTerm(_))

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

object Helper {

  /**
    * Divide the matrix into blocks. Assign a CPU core to each block.
    *
    * @return
    */
  def _concurrentActionOnSeq[T, R](list : IndexedSeq[T],
                                   action : T => R,
                                   maxCores : Int = Runtime.getRuntime.availableProcessors) = {

    val cores = Math.min(Runtime.getRuntime().availableProcessors(), maxCores)
    val n = list.length
    val max = if (n % cores == 0) { cores - 1 } else { cores }
    val stripWidth = n / cores

    // when n = 10, cores = 3 => max = 2, width = 3, strips = (0, 3) (3, 6) (6, 9) (9, 10)
    // when n = 10, cores = 2 => max = 1, width = 5, strips = (0, 5), (5, 10)

    val strips = (0 to max).map(i => (i * stripWidth, Math.min((i + 1) * stripWidth, n)))

    val calculation = strips.map(strip => {

      val range = strip._1 to strip._2 - 1

      println("Starting", strip)

      Future(range.map(i => {action(list(i))}))
    })

    Future.sequence(calculation).map(_ flatten)
  }
}
