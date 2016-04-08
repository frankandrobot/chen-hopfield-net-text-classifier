package com.frankandrobot.chen.indexer

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

  /**
    * Returns a list of terms that occur in the doc
    *
    * @param doc
    * @return
    */
  def indexConsecutiveTerms(doc : String) : List[String] = {

    return _consecutiveTerms(index(doc))
  }

  def index(doc : String) = {

    List(doc)
      .map(_removeUrls)
      .map(_removeHashtagSymbol)
      .map(tokenizer.tokenize)
      .head
      .view.zipWithIndex
      .filter(stopWords.isNotStopWord)
      .filter(_filterNonAlphanumeric)
      .filter(_filterDigits)
      .filter(_filterShortTokens)
      .map(stemmer.stem)
  }


  /**
    * Remove urls from document
    *
    * @param doc
    * @return
    */
  private def _removeUrls(doc : String) = {

    val URL = "(http|https|ftp)://\\S+".r

    URL replaceAllIn(doc, "")
  }

  /**
    * Remove hashtag symbol from document (but leave word)
    *
    * @param doc
    * @return
    */
  private def _removeHashtagSymbol(doc : String) = {

    "#".r replaceAllIn(doc, "")
  }

  /**
    * Don't really remember what the use case is...
    * If has a nonalpha-numeric character, filter out token.
    *
    * @param token
    * @return
    */
  private def _filterNonAlphanumeric(token : TokenType) : Boolean = {

    val nonalphanumeric = new Regex("[^a-zA-Z0-9]")

    nonalphanumeric.findFirstIn(token._1) match {
      case Some(x) => false
      case None => true
    }
  }

  /**
    * Filter out tokens that are digits
    * @param token
    * @return
    */
  private def _filterDigits(token : TokenType) : Boolean = {

    val number = new Regex("^[0-9]+$")

    number findFirstIn token._1 match {
      case Some(x) => false
      case _ => true
    }
  }

  private def _filterShortTokens(token : TokenType) = token._1.length >= 2


  type TokenType = (String, Int)

  private def _consecutiveTerms(tokens : Seq[TokenType]) : List[String] = {

    val tokenTypeList = tokens.foldLeft(List[TokenType]())(_concatTerms)

    return tokenTypeList.map(_._1)
  }

  private def _concatTerms(total : List[TokenType], cur : TokenType) : List[TokenType] = {

    total match {
      case Nil => cur :: Nil
      case a :: Nil => {

        val x = _concatTerms(cur, a)

        x match {
          case Some(x) => cur :: x :: total
          case _ => cur :: total
        }
        // returns cur :: _concatTerms(cur, a) :: total
      }
      case a :: b :: rest => {

        val x = _concatTerms(cur, a)
        val y = _concatTerms(cur, b)

        val list = y match {
          case Some(y) => y :: total
          case _ => total
        }

        x match {
          case Some(x) => cur :: x :: list
          case None => cur :: list
        }
        //returns cur :: _concatTerms(cur, a) :: _concatTerms(cur, b) :: total
      }
    }
  }

  private def _concatTerms(a : TokenType, b : TokenType) : Option[TokenType] =
    if (b._2 == a._2 - 1) { Some((b._1 + " " + a._1, a._2)) }
    else { None }
}

class Tokenizer {

  lazy val is = new FileInputStream("src/resources/models/en-token.bin")
  lazy val model = new TokenizerModel(is)
  lazy val tokenizer = new TokenizerME(model)

  def tokenize(doc : String) : List[String] = {

    return tokenizer.tokenize(doc).map(_ toLowerCase).toList
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

  def stem(token : (String, Int)) : (String, Int) = (stemmer.stem(token._1), token._2)
}

/*object HelloWorld extends App {

  val i = new Indexer

  //println(i.indexConsecutiveTerms("Some languages (like Haskell) are lazy: every expression’s evaluation waits for its (first) use.").toString)
  println(i.indexConsecutiveTerms("one two, three four").reverse.toString)
  println(i.indexConsecutiveTerms("one two, three four").length)
}*/
