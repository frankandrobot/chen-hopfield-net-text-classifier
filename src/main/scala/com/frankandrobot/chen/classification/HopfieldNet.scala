package com.frankandrobot.chen.classification

import com.frankandrobot.chen.DocTypes.Term
import com.frankandrobot.chen.cluster.ClusterWeights
import com.frankandrobot.chen.docs.TermStore
import com.frankandrobot.chen.indexer.Indexer

import scala.math.exp

class HopfieldNet(termStore: TermStore, clusterWeights: ClusterWeights, indexer: Indexer) {

  private def n() = termStore.terms.length

  def relatedConcepts(theta_j : Double, theta_o : Double, epsilon : Double)(word : String) = {

    indexer.index(word) match {

      case Nil => Nil
      case foo :: rest => {

        _lookup(foo) match {

          case None => Nil
          case Some(term) => {

            val _mu = mu(theta_j, theta_o, term.index)

            val error = (t : Int) => (0 to n - 1).foldLeft(0.0){ (total, j) =>

              val x = _mu(j, t + 1) - _mu(j, t)
              x * x
            }

            var t = 0
            while(error(t) > epsilon) { t += 1 }

            (0 to n - 1).foldLeft(List[Term]()){ (total, i) => {

              _mu(i, t + 1) match {
                case m if m > 0 => termStore.terms()(i) :: total
                case _ => total
              }
            }}
          }
        }
      }
    }
  }

  private def _lookup(term : String) = termStore.termMap.get(term)

  private def mu(theta_j : Double, theta_o : Double, inputIndex : Int)(j : Int, t : Int) : Double = {

    t match {
      case 0 => if (j == inputIndex) {1.0} else {0.0}
      case t if t > 0 => {

        val _mu = mu(theta_j, theta_o, inputIndex)
        val _fs = fs(theta_j, theta_o)

        val sum = (0 to n - 1).foldLeft(0.0)((total, i) => total + clusterWeights.weights(i, j) * _mu(i, t - 1))

        _fs(sum)
      }
    }
  }

  private def fs(theta_j : Double, theta_o : Double)(net : Double) = 1.0 / (1.0 + exp((-(net - theta_j) / theta_o)))
}
