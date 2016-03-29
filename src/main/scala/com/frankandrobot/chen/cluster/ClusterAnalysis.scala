package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.{DocWithRawTerms, RawTerm}
import com.frankandrobot.chen.docs.RawTermsByDocStore
import com.frankandrobot.chen.utils.Memoize._

import scala.annotation.tailrec

// TODO use lazy views
// TODO replace nested cases with monads

class ClusterAnalysis(docStore : RawTermsByDocStore) {

  def termFrequency(docWithRawTerms: DocWithRawTerms, rawTerm : RawTerm) : Int = {

    val histogram = _histogramFn(docWithRawTerms)

    histogram.getOrElse(rawTerm.value, 0)
  }

  def termFrequency(docWithRawTerms: DocWithRawTerms, rawTerm1 : RawTerm, rawTerm2 : RawTerm) = {

    termFrequency(docWithRawTerms, rawTerm1) + termFrequency(docWithRawTerms, rawTerm2)
  }

  /**
    * For each doc,
    *    docFreq += termFreq(rawTerm, doc) ? 1 : 0
    *
    * @param rawTerm
    * @return
    */
  def docFrequency(rawTerm : RawTerm) : Int = {

    docStore.docs.foldLeft(0) { (total, cur) => total + (if (termFrequency(cur, rawTerm) > 0) {1} else {0}) }
  }

  def docFrequency(rawTerm1 : RawTerm, rawTerm2 : RawTerm) = {

    docStore.docs.foldLeft(0) { (total, cur) => {

      val term1Occurs = termFrequency(cur, rawTerm1) > 0
      lazy val term2Occurs = termFrequency(cur, rawTerm2) > 0

      total + (if (term1Occurs && term2Occurs) {1} else {0})
    }}
  }

  private def _histogram(docWithRawTerms: DocWithRawTerms) = {

    val docTerms = docWithRawTerms.terms

    docTerms.foldLeft(Map[String, Int]()) { (total, cur) => total + (cur.value -> (1 + total.getOrElse(cur.value, 0))) }
  }

  private val _histogramFn = memoize(_histogram _)

  @tailrec
  final def analyze(target : Float = 0.90f,
                    threshold : Int = 1,
                    prevDocs : List[DocWithRawTerms] = docStore.docs,
                    prevDiff : Float = 0) : List[DocWithRawTerms] = {

    val curDocs = prevDocs.filter( doc => {

      val terms = doc.terms.filter(termFrequency(doc, _) >= threshold)
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
