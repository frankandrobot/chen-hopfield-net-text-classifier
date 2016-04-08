package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.{Doc, RawTerm, Term}
import com.frankandrobot.chen.docs.{DocStore, TermStore}

import scala.math.log10


class ClusterWeights(termStore: TermStore,
                     clusterAnalysis: ClusterAnalysis,
                     rawTermsByDocStore: DocStore) {

  /**
    * Note that this doesn't return the right answer when the terms are identical
    *
    * @param doc
    * @param rawTerm1
    * @param rawTerm2
    * @return
    */
  private def _termFrequency(doc: Doc, rawTerm1 : RawTerm, rawTerm2 : RawTerm) : Int = {

    clusterAnalysis.termFrequency(doc, rawTerm1) + clusterAnalysis.termFrequency(doc, rawTerm2)
  }

  /**
    * Note that this doesn't return the right answer when the terms are identical
    *
    * @param rawTerm1
    * @param rawTerm2
    * @return
    */
  private def _docFrequency(rawTerm1 : Term, rawTerm2 : Term) = {

    (rawTerm1.docs intersect rawTerm2.docs).size
    /*FMath.sum(clusterAnalysis.docStore.docs, (doc : Doc) => {

      if (clusterAnalysis.termFrequency(doc, rawTerm1) > 0 && clusterAnalysis.termFrequency(doc, rawTerm2) > 0) {1} else {0}
    })*/
  }

  def weight(doc : Doc, termIndex : Int) : Double = {

    val term = termStore.terms()(termIndex)

    clusterAnalysis.termFrequency(doc, term) * log10(clusterAnalysis.docFrequency(term))
  }

  def weight(doc: Doc, termIndex1 : Int, termIndex2 : Int) : Double = {

    if (termIndex1 != termIndex2) {

      val term1 = termStore.terms()(termIndex1)
      val term2 = termStore.terms()(termIndex2)

      _termFrequency(doc, term1, term2) * log10(_docFrequency(term1, term2))
    }
    else {

      weight(doc, termIndex1)
    }
  }

}
