package com.frankandrobot.chen

import java.io.File

import com.frankandrobot.chen.DocTypes.{DocId, RawDoc}
import com.frankandrobot.chen.classification._
import org.json4s.jackson.JsonMethods._
import org.json4s.{DefaultFormats, _}

import scala.io.Source


case class Tweet(id_str : String, text : String)


object Main extends App {

  implicit val formats = DefaultFormats

  def getListOfFiles(dir : String) : List[File] = new File(dir).listFiles.filter(_.isFile).toList

  // load files
  val tweets = getListOfFiles("src/resources/data/tweets")
  val parsed = tweets
    .map(filename => Source.fromFile(filename).getLines.drop(1).mkString)
    .map(contents => parse(contents))
    .map(json => json.extract[List[Tweet]])
    .flatten
    .take(85)

  val params = Params(
    DocIndexingTarget(0.9),
    HopfieldNetParams(ThetaJ(0.1), ThetaO(0.01), ErrorEpsilon(1.0))
  )

  val chenHopfieldNet = new ChenHopfieldNet(params)

  chenHopfieldNet
    .addDocs(parsed, (t : Tweet) => RawDoc(DocId(t.id_str), "", t.text))
    .infoLossAnalysis()
    .calculateWeights()

  println(chenHopfieldNet.stats)

  println("---------------")
  var result = chenHopfieldNet.lookup("big companies")
  result.toList.foreach(println)

  println("---------------")
  result = chenHopfieldNet.lookup("andi warhol")
  result.toList.foreach(println)

  println("---------------")
  result = chenHopfieldNet.lookup("linux")
  result.toList.foreach(println)

  println("---------------")
  result = chenHopfieldNet.lookup("reactjs")
  result.toList.foreach(println)
}
