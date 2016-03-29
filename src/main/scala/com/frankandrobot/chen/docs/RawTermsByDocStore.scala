package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.DocWithRawTerms


/**
  * The raw terms grouped by doc
  */
class RawTermsByDocStore {

  private var _docs = List[DocWithRawTerms]()

  def docs() = _docs

  def add(docRawTerms : DocWithRawTerms) : Unit = {

    _docs = docRawTerms :: _docs
  }

  def add(rawTermsByDoc: Seq[DocWithRawTerms]) : Unit = {

    rawTermsByDoc.foreach(add)
  }
}
