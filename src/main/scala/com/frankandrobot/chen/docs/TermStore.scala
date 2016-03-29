package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.{DocLite, DocWithRawTerms, Term}
import com.frankandrobot.chen.clusteranalysis.ClusterAnalysis

import scala.collection.mutable.{ArrayBuffer, HashMap}

class TermStore(clusterAnalysis: ClusterAnalysis) {

  private var _termList = ArrayBuffer.empty[Term]
  private var _termMap = HashMap.empty[String, Term]

  def terms() = _termList

  def add(rawTermsByDoc : Seq[DocWithRawTerms]) = {

    rawTermsByDoc.foreach(docRawTerms => {

      docRawTerms.terms.foreach(term => {

        _termMap.get(term.value) match {
          case Some(x) => _termMap += (term.value -> Term(x.value, x.docs + docRawTerms.doc))
          case _ => {
            val newTerm = Term(term.value, Set[DocLite](docRawTerms.doc))
            _termMap += (term.value -> newTerm)
            _termList += newTerm
          }
        }
      })
    })
  }

  def sort() = {
    _termList = _termList.sortWith(clusterAnalysis.docFrequency(_) < clusterAnalysis.docFrequency(_))
    _termList
  }
}
