package com.frankandrobot.chen.docs

import scala.collection.mutable


class RawDocStore {

  val docs = mutable.Map.empty[DocId, String]

  def add(doc : String) = {

    docs.put(DocId(docs.size), doc)
  }
}
