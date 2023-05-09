package quickcheck

import org.scalacheck.*
import Arbitrary.*
import Gen.*
import Prop.forAll
import java.lang.Math.min

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap:
  lazy val genHeap: Gen[H] = oneOf(
    const(empty),
    for {
      v <- arbitrary[A]
      h <- oneOf(const(empty), genHeap)
    } yield insert(v, h)
  )
  given Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if isEmpty(h) then 0 else findMin(h)
    findMin(insert(m, h)) == m
  }

  property("min2") = forAll { (a: Int, b: Int) =>
    val h = insert(b, insert(a, empty))
    findMin(h) == (if a < b then a else b)
  }
  
  property("del") = forAll { (a: Int) =>
    val h = insert(a, empty)
    isEmpty(deleteMin(h))
  }

  property("orderx") = forAll { (h: H) =>
    def help(h: H): Boolean = {
      if isEmpty(h) then
        true
      else
        val min = findMin(h)
        val newH = deleteMin(h)
        if isEmpty(newH) then
          true
        else
          min <= findMin(newH) && help(newH)
    }
    help(h)
  }

  property("min of 2") = forAll { (h1: H, h2: H) =>
    if isEmpty(h1) || isEmpty(h2) then
      true
    else
      findMin(meld(h1, h2)) == min(findMin(h1), findMin(h2))
  }

  property("Check two heaps equal") = forAll { (ha: H, hb: H) =>
    def equal(h1: H, h2: H): Boolean =
      if (isEmpty(h1) && isEmpty(h2)) true
      else if (isEmpty(h1) || isEmpty(h2)) false
      else {
        val m1 = findMin(h1)
        val m2 = findMin(h2)
        m1 == m2 && equal(deleteMin(h1), deleteMin(h2))
    }
    if isEmpty(ha) then
      true
    else
      equal(meld(ha, hb), meld(deleteMin(ha), insert(findMin(ha), hb)))
  }