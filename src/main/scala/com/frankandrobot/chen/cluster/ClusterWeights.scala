package com.frankandrobot.chen.cluster

import com.frankandrobot.chen.DocTypes.Doc
import com.frankandrobot.chen.docs.{DocStore, TermStore}

import scala.math.log10


class ClusterWeights(termStore: TermStore,
                     clusterAnalysis: ClusterAnalysis,
                     rawTermsByDocStore: DocStore) {

  def weight(doc : Doc, termIndex : Int) = {

    val term = termStore.terms()(termIndex)

    clusterAnalysis.termFrequency(doc, term) * log10(clusterAnalysis.docFrequency(term))
  }

  def weight(doc: Doc, termIndex1 : Int, termIndex2 : Int) = {

    val term1 = termStore.terms()(termIndex1)
    val term2 = termStore.terms()(termIndex2)

    clusterAnalysis.termFrequency(doc, term1, term2) * log10(clusterAnalysis.docFrequency(term1, term2))
  }

}
