package com.frankandrobot.chen.docs

import com.frankandrobot.chen.DocTypes.Doc


/**
  * The raw terms grouped by doc
  */
class DocStore {

  private var _docs = List[Doc]()

  def docs() = _docs

  def add(docRawTerms : Doc) : Unit = {

    _docs = docRawTerms :: _docs
  }

  def add(rawTermsByDoc: Seq[Doc]) : Unit = {

    rawTermsByDoc.foreach(add)
  }
}
