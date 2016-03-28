package com.frankandrobot.chen.docs

import com.frankandrobot.chen.terms.RawTerm


case class DocId(val id : Int)

case class Doc(val docId : DocId,
               val terms: List[RawTerm])

