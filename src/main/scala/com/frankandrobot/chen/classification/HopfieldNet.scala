package com.frankandrobot.chen.classification

import com.frankandrobot.chen.DocTypes.Term
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.TermStore
import com.frankandrobot.chen.indexer.Indexer

import scala.math.exp


case class HopfieldNetParams(theta_j : ThetaJ,
                             theta_o : ThetaO,
                             errorEpsilon: ErrorEpsilon)

case class ThetaJ(value : Double)
case class ThetaO(value : Double)
case class ErrorEpsilon(value : Double)


class HopfieldNet(termStore: TermStore,
                  clusterWeights: ClusterWeights,
                  connectionWeights: ConnectionWeights,
                  indexer: Indexer) {

  private val defaultParams = HopfieldNetParams(ThetaJ(0.1), ThetaO(0.01), ErrorEpsilon(1.0))

  private def n() = termStore.terms.length


  /**
    * Doesn't work on phrases.
    * Doesn't work on words that haven't been indexed.
    *
    * @param word
    * @param params
    * @return
    */
  def simpleFindRelatedTerms(word : String, params: HopfieldNetParams = defaultParams) = {

    indexer.index(word).map(_._1).force match {

      case Nil => Nil
      case foo => {

        _lookup(foo) match {

          case None => Nil
          case Some(term) => {

            val theta_j = params.theta_j.value
            val theta_o = params.theta_o.value
            val epsilon = params.errorEpsilon.value

            val _mu : (Int, Int) => Double = mu(theta_j, theta_o, term.index)

            val error = (t : Int) => List.tabulate(n - 1){ j =>

              val x = _mu(j, t) - _mu(j, t - 1)
              x * x

            }.sum

            var t = 1

            while(error(t) > epsilon) { t += 1; }

            val matches = (0 to n - 1).foldLeft(List[Term]()){ (total, i) => {

              _mu(i, t) match {
                case m if m > 0 => termStore.terms()(i) :: total
                case _ => total
              }
            }}

            matches
          }
        }
      }
    }
  }

  private def _extractDocs(list : Seq[Term]) = {

    //val myOrdering = Ordering.fromLessThan[(Term, Int)](_._2 > _._2)
    //val docs = TreeSet.empty(myOrdering)

    list.flatMap(term => term.docs.toSet).toSet
  }

  private def _lookup(term : Seq[String]) = { termStore.termMap.get(term.mkString(" ")) }

  private def mu(theta_j : Double, theta_o : Double, inputIndex : Int)(j : Int, t : Int) : Double = {

    t match {
      case 0 => if (j == inputIndex) {1.0} else {0.0}
      case t if t > 0 => {

        val _mu : (Int, Int) => Double = mu(theta_j, theta_o, inputIndex)
        val _fs : Double => Double = fs(theta_j, theta_o)

        val sum = List.tabulate(n - 1)(i => connectionWeights.weights()(i, j) * _mu(i, t - 1)).sum
        // val sum = (0 to n - 1).foldLeft(0.0)((total, i) => total + connectionWeights.weights()(i, j) * _mu(i, t - 1))

        _fs(sum)
      }
    }
  }

  private def fs(theta_j : Double, theta_o : Double)(net : Double) = 1.0 / (1.0 + exp((-(net - theta_j) / theta_o)))
}
