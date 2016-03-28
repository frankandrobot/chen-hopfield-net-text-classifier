package com.frankandrobot.chen.docs


class DocStore {

  var docs = List[Doc]()

  def add(doc : Doc) : Unit = {

    docs = doc :: docs
  }
}
