package com.frankandrobot.indexer

import java.io.FileInputStream

import opennlp.tools.stemmer.PorterStemmer
import opennlp.tools.tokenize.{TokenizerME, TokenizerModel}

import scala.io.Source
import scala.util.matching.Regex


class Indexer {

  val tokenizer = new Tokenizer
  val stopWords = new StopWords
  val stemmer = new Stemmer

  val termSize = 3

  def index(str :String) : List[String] = {

    val iteratee = tokenizer.tokenize(str)
      .view.zipWithIndex
      .filter(stopWords.isNotStopWord)
      .filter(_filterOutNonAlphanumeric)
      .map(stemmer.stem)
      .sliding(termSize)
      //.map(f => {println(f.toList.toString); f})
      .foldLeft(List[String]())(_collectTerms)

    return iteratee
  }

  private def _filterOutNonAlphanumeric(word : (String, Int)) : Boolean = {

    val nonalphanumeric = new Regex("[^a-zA-Z0-9]")

    return nonalphanumeric.findFirstIn(word._1) match {
      case Some(x) => false
      case None => true
    }
  }

  /**
    * Ex: [1] => 1
    * Ex: [1,2] => 1 2
    * Ex: [1,2,3] => 1 2 3
    *
    * @param list
    * @return
    */
  private def _concat(list : Seq[String]) : String = {

    return list.reverse mkString " "
  }

  /**
    * Build terms.
    *
    * A "term" is every consecutive sequence of every length in the list.
    *
    * Ex: (1,2,3) => (1,2,3,12,23,123)
    *
    * @param list
    * @return
    */
  private def _buildTerms(list : List[String]) : List[String] = {

    return (1 to list.length).foldLeft(List[String]()) { (total, cur) =>

      val allConsecutiveSeqsOfSizeCur = list.sliding(cur).toList
      val terms = allConsecutiveSeqsOfSizeCur.map(_concat)

      terms.toList ++ total
    }
  }

  private def _consecutiveTerms(list : Seq[(String, Int)]) : List[String] = {

    val iteratee = list.zipWithIndex.foldLeft(Iteratee()){ (total, cur) => {

      val curTerm = cur._1._1
      val curPos = cur._1._2
      val curPosInList = cur._2

      total.prevPos match {
        case n if (n == curPos - 1 && curPosInList < list.length - 1) =>
          Iteratee(prevPos = curPos, rawTerms = curTerm :: total.rawTerms)
        case n if (n == curPos - 1 && curPosInList == list.length - 1) =>
          Iteratee(terms = _buildTerms(curTerm :: total.rawTerms) ++ total.terms, prevPos = curPos)
        case _ =>
          Iteratee(terms = _buildTerms(total.rawTerms) ++ total.terms, prevPos = curPos, rawTerms = List(curTerm))
      }
    }}

    return iteratee.rawTerms ++ iteratee.terms
  }

  private def _collectTerms(total : List[String], tokens : Seq[(String, Int)]) : List[String] = {

    return _consecutiveTerms(tokens) ++ total
  }
}

case class Iteratee(val terms : List[String] = List[String](),
                    val prevPos : Int = -1,
                    val rawTerms : List[String] = List[String]())

// 1
// 2
// 12
// {123, 23} {12, 2}

class Tokenizer {

  lazy val is = new FileInputStream("src/resources/models/en-token.bin")
  lazy val model = new TokenizerModel(is)
  lazy val tokenizer = new TokenizerME(model)

  def tokenize(str : String) : List[String] = {

    return tokenizer.tokenize(str).map(_ toLowerCase).toList
  }
}

class StopWords {

  lazy val stopwords = Source.fromFile("src/resources/stopwords/english").getLines.toList
  lazy val stopWordsMap = stopwords.foldLeft(Map[String,Boolean]()) {(total, word) => total ++ Map(word -> true)}

  def isStopWord(word : (String, Int)): Boolean = stopWordsMap.getOrElse(word._1, false)
  def isNotStopWord(word : (String, Int)) = !isStopWord(word)
}

class Stemmer {

  lazy val stemmer = new PorterStemmer()

  def stem(str : (String, Int)) : (String, Int) = (stemmer.stem(str._1), str._2)
}

object HelloWorld extends App {

  val i = new Indexer

  //println(i.index("Some languages (like Haskell) are lazy: every expression’s evaluation waits for its (first) use.").toString)
  println(i.index("lazy languages Haskell evaluation").reverse.toString)

}
