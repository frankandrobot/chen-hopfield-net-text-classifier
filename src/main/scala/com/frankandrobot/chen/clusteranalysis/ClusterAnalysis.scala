package com.frankandrobot.chen.clusteranalysis

import com.frankandrobot.chen.docs.{Doc, DocStore}
import com.frankandrobot.chen.terms.RawTerm
import com.frankandrobot.chen.utils.Memoize._

import scala.annotation.tailrec

// TODO use lazy views
// TODO replace nested cases with monads

class ClusterAnalysis(DocStore : DocStore) {

  def termFrequency(rawTerm : RawTerm, doc : Doc) : Int = {

    val histogram = _histogramFn(doc)

    histogram(rawTerm.value)
  }

  /**
    * For each doc,
    *    docFreq += termFreq(rawTerm, doc)
    *
    * @param rawTerm
    * @return
    */
  def docFrequency(rawTerm : RawTerm) : Int = {

    DocStore.docs.foldLeft(0) { (total, cur) => total + termFrequency(rawTerm, cur) }
  }

  private def _histogram(doc : Doc) = {

    val docTerms = doc.terms

    docTerms.foldLeft(Map[String, Int]()) { (total, cur) => total + (cur.value -> (1 + total.getOrElse(cur.value, 0))) }
  }

  private val _histogramFn = memoize(_histogram _)

  @tailrec
  final def analyze(target : Float = 0.90f, threshold : Int = 1, prevDocs : List[Doc] = DocStore.docs, prevDiff : Float = 0)
  : List[Doc] = {

    val curDocs = prevDocs.filter( doc => {

      val terms = doc.terms.filter(termFrequency(_, doc) >= threshold)
      terms.head != Nil
    })

    val ratio = curDocs.length.toFloat / prevDocs.length.toFloat

    val diff = ratio - target

    if (diff <= 0) {
      if (prevDiff < diff) { return prevDocs }
      else { return curDocs }
    }

    return analyze(target, threshold + 1, curDocs, diff)
  }
}
