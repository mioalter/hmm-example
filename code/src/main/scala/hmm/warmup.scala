
package hmm

import com.cra.figaro.language._

object warmUp {
    // II. Our HMM

    sealed trait Hidden
    case object Rainy extends Hidden
    case object Sunny extends Hidden
    
    sealed trait Observable
    case object Shop extends Observable
    case object Clean extends Observable
    case object Walk extends Observable
    

    // III. Probabilistic Functional Programming

    type Dist[A] = List[(A, Double)]

    def transitionGivenState(h : Hidden) : Dist[Hidden] = {
        h match {
            case Rainy => List((Rainy, 0.7), (Sunny, 0.3)) // P(H_t | R_{t-1})
            case Sunny => List((Rainy, 0.2) , (Sunny, 0.8)) // P(H_t | S_{t-1})
        }
    }

    // IV. An HMM in Figaro

    val d : Dist[Hidden] = List((Rainy, 0.1), (Sunny, 0.9)) // List(P(R_{t-1}), P(S_{t-1}))
    
    val fd : Element[Hidden] = Select(0.1 -> Rainy, 0.9 -> Sunny)    

}