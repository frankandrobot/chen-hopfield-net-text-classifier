package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.{Doc, RawTerm}
import com.frankandrobot.chen.docs.DocStore
import com.frankandrobot.chen.utils.FMath
import com.frankandrobot.chen.utils.Memoize._

import scala.annotation.tailrec

// TODO use lazy views
// TODO replace nested cases with monads

class ClusterAnalysis(val docStore : DocStore) {

  def termFrequency(doc: Doc, rawTerm : RawTerm) : Int = {

    doc.histogram.getOrElse(rawTerm.value, 0)
  }

  /**
    * For each doc,
    *    docFreq += termFreq(rawTerm, doc) ? 1 : 0
    *
    * @param rawTerm
    * @return
    */
  def docFrequency(rawTerm : RawTerm) : Int = {

    _docFrequency(rawTerm)
  }

  private def _docFrequency(rawTerm : RawTerm) : Int = {

    FMath.sum(docStore.docs, (doc : Doc) => { if (termFrequency(doc, rawTerm) > 0) {1} else {0}} )
  }

  private def _docFrequencyFn = memoize(_docFrequency _)

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
                             prevDocs : List[Doc] = docStore.docs,
                             prevDiff : Double = 0) : List[Doc] = {

    val curDocs = prevDocs.filter( doc => {

      val terms = doc.terms.filter(docFrequency(_) >= docFrequencyThreshold)
      terms.length > 0
    })

    val ratio = curDocs.length.toDouble / docStore.docs.length.toDouble

    // This is decreasing, since you always remove more terms in each step
    val diff = ratio - docIndexingTarget

    if (diff <= 0) {
      if (prevDiff < diff) { return prevDocs }
      else { return curDocs }
    }

    return infoLossAnalysis(docIndexingTarget, docFrequencyThreshold + 1, curDocs, diff)
  }
}
