package org.geneticmachine.navigation

package object utils {
  def logistic(alpha: Double)(x: Double): Double = {
    1.0 / (1 + math.exp(-x * alpha))
  }
}
