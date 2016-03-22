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

    return _consecutiveTerms(iteratee)
  }

  private def _filterOutNonAlphanumeric(word : (String, Int)) : Boolean = {

    val nonalphanumeric = new Regex("[^a-zA-Z0-9]")

    return nonalphanumeric.findFirstIn(word._1) match {
      case Some(x) => false
      case None => true
    }
  }

  type TokenType = (String, Int)

  private def _consecutiveTerms(tokens : Seq[TokenType]) : List[String] = {

    val tokenTypeList = tokens.foldLeft(List[TokenType]())(_concatTerms)

    return tokenTypeList.map(_._1)
  }

  private def _concatTerms(total : List[TokenType], cur : TokenType) : List[TokenType] = {

    total match {
      case Nil => cur :: Nil
      case a :: Nil => cur :: _concatTerms(cur, a) :: total
      case a :: b :: rest => cur :: _concatTerms(cur, a) :: _concatTerms(cur, b) :: total
    }
  }

  private def _concatTerms(a : TokenType, b : TokenType) = (b._1 + " " + a._1, a._2)

}

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
  println(i.index("one two three four").reverse.toString)
  println(i.index("one two three four").length)
}
