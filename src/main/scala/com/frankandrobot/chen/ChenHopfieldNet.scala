package com.frankandrobot.chen

import com.frankandrobot.chen.DocTypes.RawDoc
import com.frankandrobot.chen.classification.{ConnectionWeights, HopfieldNet, HopfieldNetParams}
import com.frankandrobot.chen.cluster.{ClusterAnalysis, ClusterWeights}
import com.frankandrobot.chen.docs.{DocStore, RawDocStore, TermStore}
import com.frankandrobot.chen.indexer.Indexer


class ChenHopfieldNet(params: Params) {

  private val indexer = new Indexer()
  private val rawDocStore = new RawDocStore(indexer)
  private val docStore = new DocStore
  private val clusterAnalysis = new ClusterAnalysis(docStore)
  private val termStore = new TermStore(clusterAnalysis)
  private val clusterWeights = new ClusterWeights(termStore, clusterAnalysis, docStore)
  private val connectionWeights = new ConnectionWeights(termStore, docStore, clusterWeights)
  private val hopfieldNet = new HopfieldNet(termStore, clusterWeights, connectionWeights, indexer)

  private var matrixCalculateTime : Long = 0

  /**
    * Yea, this also computes the histograms. Should probably be made explicit
    *
    * @param docs
    * @param fn
    * @tparam T
    */
  def addDocs[T](docs : Seq[T], fn : T => RawDoc) = {

    docs.foreach(doc => rawDocStore.add(fn(doc)))
    docStore.add(rawDocStore.toDocs())

    this
  }

  def infoLossAnalysis() = {

    val newDocs = clusterAnalysis.infoLossAnalysis(params.docIndexingTarget.value)
    docStore.replace(newDocs)
    termStore.add(newDocs)
    termStore.sort()

    this
  }

  def calculateWeights() = {

    val startTime = System.currentTimeMillis()
    connectionWeights.weights()
    val endTime = System.currentTimeMillis()

    matrixCalculateTime = endTime - startTime

    this
  }

  def stats() = Stats(
    DocCount(docStore.docs.length),
    TermCount(termStore.terms.length),
    MatrixCalculationTime(matrixCalculateTime),
    params
  )

  def lookup(term : String) = hopfieldNet.simpleFindRelatedTerms(term, params.hopfieldNetParams).take(20)
}

case class Params(docIndexingTarget : DocIndexingTarget,
                  hopfieldNetParams: HopfieldNetParams)
case class DocIndexingTarget(value : Double = 0.9)

case class Stats(docCount : DocCount,
                 termCount : TermCount,
                 matrixCalculationTime : MatrixCalculationTime,
                 params: Params)
case class DocCount(value : Int)
case class TermCount(value : Int)
case class MatrixCalculationTime(value : Long)