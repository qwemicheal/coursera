package quickcheck

import common._

import org.scalacheck._
import Arbitrary._
import Gen._
import Prop._

abstract class QuickCheckHeap extends Properties("Heap") with IntHeap {

  lazy val genHeap: Gen[H] = for {
    m <- arbitrary[Int]
    ho <- oneOf (const(empty), genHeap)
  }yield insert(m,ho)



  implicit lazy val arbHeap: Arbitrary[H] = Arbitrary(genHeap)

  property("gen1") = forAll { (h: H) =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m, h)) == m
  }


  property("min1") = forAll { a: Int =>
    val h = insert(a, empty)
    findMin(h) == a
  }

  property("min2") = forAll { h: H =>
    val m = if (isEmpty(h)) 0 else findMin(h)
    findMin(insert(m-1,insert(m-3,insert(m-3,insert(m-1, h))))) == m-3
  }
  property("add,then delete") = forAll { (h: H) =>
    if (isEmpty(h))
      { val a=6
        isEmpty(deleteMin(insert(a, h)))==true
      }
    else {
      val a=findMin(h)-1
      findMin(insert(a, h)) == a
    }
  }
  property("insert two and find the minimum") = forAll { (x: Int, y: Int) =>
    val heap = insert(x, insert(y, empty))
    findMin(heap) == math.min(x, y)
  }

  property("finding minimum of melding") = forAll { (h: H,h2:H) =>
    if (isEmpty(h)&isEmpty(h2))
      isEmpty(meld(h,h2))==true
    else if (!(isEmpty(h)|isEmpty(h2)))
    findMin(meld(h,h2))==(findMin(h)|findMin(h2))
    else if(isEmpty(h))
      findMin(meld(h,h2))==(findMin(h2))
    else findMin(meld(h,h2))==(findMin(h))
  }

  property("resursive find min and delete") = forAll { (h:H)=>
    var l:List[Int]=List()
    def findsmall(h_left:H):List[Int]={
      if (isEmpty(h_left)) List()
      else {
        val s=findMin(h_left)
        l=s::l;
        s::findsmall(deleteMin(h_left))
      }
    }
    findsmall(h).sorted==l
  }

}
