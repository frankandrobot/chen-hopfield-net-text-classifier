package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{Doc, DocLite, Term}
import com.frankandrobot.chen.cluster.ClusterAnalysis

import scala.collection.mutable.{ArrayBuffer, HashMap}

class TermStore(clusterAnalysis: ClusterAnalysis) {

  private var _termList = ArrayBuffer.empty[Term]
  private var _termMap = HashMap.empty[String, Term]

  def terms() = _termList
  def termMap() = _termMap

  def add(rawTermsByDoc : Seq[Doc]) = {

    rawTermsByDoc.foreach(docRawTerms => {

      docRawTerms.terms.foreach(term => {

        _termMap.get(term.value) match {
          case Some(x) => _termMap += (term.value -> Term(x.index, x.value, x.docs + docRawTerms.doc))
          case _ => {
            val newTerm = Term(_termList.length, term.value, Set[DocLite](docRawTerms.doc))
            _termMap += (term.value -> newTerm)
            _termList += newTerm
          }
        }
      })
    })
  }

  def sort() = {
    _termList = _termList.sortWith(clusterAnalysis.docFrequency(_) > clusterAnalysis.docFrequency(_))
    _termList
  }
}
