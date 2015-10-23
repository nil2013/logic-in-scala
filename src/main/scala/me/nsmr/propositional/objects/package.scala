package me.nsmr.propositional

/**
 * Provides prolog engines
 */
package object objects {
  type -->[F, T] = PartialFunction[F, T]
  object SingleIterator {
    def apply[T](obj: T) = {
      new Iterator[T] {
        var flag = true
        def hasNext = {
          if(flag) {
            flag = false
            true
          } else {
            false
          }
        }
        def next = obj
      }
    }
  }
}