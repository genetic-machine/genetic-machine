package org.geneticmachine.genetic

import scala.collection.parallel.immutable.ParRange

package object evolution {
  final def parRange(from: Int, until: Int): ParRange = {
    from.until(until).par
  }
}
