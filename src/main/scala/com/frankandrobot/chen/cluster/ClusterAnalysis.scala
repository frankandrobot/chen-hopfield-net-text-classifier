package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.{Doc, RawTerm}
import com.frankandrobot.chen.docs.RawTermsByDocStore
import com.frankandrobot.chen.utils.Memoize._

import scala.annotation.tailrec

// TODO use lazy views
// TODO replace nested cases with monads

class ClusterAnalysis(rawTermsByDocStore : RawTermsByDocStore) {

  def termFrequency(docWithRawTerms: Doc, rawTerm : RawTerm) : Int = {

    val histogram = _histogramFn(docWithRawTerms)

    histogram.getOrElse(rawTerm.value, 0)
  }

  def termFrequency(docWithRawTerms: Doc, rawTerm1 : RawTerm, rawTerm2 : RawTerm) : Int = {

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

    _docFrequencyFn(rawTerm)
  }

  def docFrequency(rawTerm1 : RawTerm, rawTerm2 : RawTerm) = {

    _docFrequency2Fn(rawTerm1, rawTerm2)
  }

  /**
    * We use a mutable structure because when dealing with 1000s of terms, it's faster
    *
    * @param docWithRawTerms
    * @return
    */
  private def _histogram(docWithRawTerms: Doc): collection.Map[String, Int] = {

    val hash = collection.mutable.HashMap.empty[String, Int] withDefaultValue 0
    docWithRawTerms.terms foreach { cur => hash(cur.value) += 1 }
    hash
  }

  private def _docFrequency(rawTerm : RawTerm) : Int = {

    rawTermsByDocStore.docs.foldLeft(0) { (total, cur) => total + (if (termFrequency(cur, rawTerm) > 0) {1} else {0}) }
  }

  private def _docFrequency2(rawTerm1 : RawTerm, rawTerm2 : RawTerm) = {

    rawTermsByDocStore.docs.foldLeft(0) { (total, cur) => {

      val termOneOccurs = termFrequency(cur, rawTerm1) > 0
      lazy val termTwoOccurs = termFrequency(cur, rawTerm2) > 0

      total + (if (termOneOccurs && termTwoOccurs) {1} else {0})
    }}
  }

  private val _histogramFn = memoize(_histogram _)
  private val _docFrequencyFn = memoize(_docFrequency _)
  private val _docFrequency2Fn = memoize(_docFrequency2 _)

  /**
    * Discard documents where all the terms have a docFrequency < docFrequencyThreshold
    * Adjust docFrequencyThreshold so that the doc indexing percent (ratio of docs lefts to original docs)
    * is close to the docIndexingTarget
    *
    * @param docIndexingTarget
    * @param docFrequencyThreshold
    * @param prevDocs
    * @param prevDiff
    * @return
    */
  @tailrec
  final def infoLossAnalysis(docIndexingTarget : Double = 0.90,
                             docFrequencyThreshold : Int = 1,
                             prevDocs : List[Doc] = rawTermsByDocStore.docs,
                             prevDiff : Double = 0) : List[Doc] = {

    val curDocs = prevDocs.filter( doc => {

      val terms = doc.terms.filter(docFrequency(_) >= docFrequencyThreshold)
      terms.length > 0
    })

    val ratio = curDocs.length.toDouble / rawTermsByDocStore.docs.length.toDouble

    // This is decreasing, since you always remove more terms in each step
    val diff = ratio - docIndexingTarget

    if (diff <= 0) {
      if (prevDiff < diff) { return prevDocs }
      else { return curDocs }
    }

    return infoLossAnalysis(docIndexingTarget, docFrequencyThreshold + 1, curDocs, diff)
  }
}
