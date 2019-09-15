package nl.codestar

package object scalatsi {
  implicit private[scalatsi] class IndexedSeqExtensions[T](val seq: IndexedSeq[T]) extends AnyVal {

    /** Find the highest common prefix index
      * @example Seq("a","b","c") and Seq("a", "b", "d") will return 2 */
    def findCommonPrefixLength(other: IndexedSeq[T]): Int = {
      var i = 0
      while (i < seq.length && i < other.length && seq(i) == other(i)) { i += 1 }
      i
    }

    def dropCommonPrefix(other: IndexedSeq[T]): IndexedSeq[T] =
      seq.drop(seq.findCommonPrefixLength(other))
  }
}
