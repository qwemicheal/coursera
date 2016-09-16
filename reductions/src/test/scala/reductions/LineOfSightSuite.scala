package reductions

import java.util.concurrent._
import scala.collection._
import org.scalatest.FunSuite
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import common._
import java.util.concurrent.ForkJoinPool.ForkJoinWorkerThreadFactory

@RunWith(classOf[JUnitRunner]) 
class LineOfSightSuite extends FunSuite {
  import LineOfSight._
  test("lineOfSight should correctly handle an array of size 4") {
    val output = new Array[Float](4)
    parLineOfSight(Array[Float](0f, 1f, 8f, 9f), output,1)
    assert(output.toList == List(0f, 1f, 4f, 4f))
  }


  test("upsweepSequential should correctly handle the chunk 1 until 4 of an array of 4 elements") {
    val res = upsweepSequential(Array[Float](0f, 1f, 8f, 15f), 1, 4)
    assert(res == 5f)
  }


    test("downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
      val output = new Array[Float](4)
      downsweepSequential(Array[Float](0f, 1f, 8f, 15f), output, 0f, 1, 4)
      assert(output.toList == List(0f, 1f, 4f, 5f))
    }
  test("2 downsweepSequential should correctly handle a 4 element array when the starting angle is zero") {
    val output = new Array[Float](4)
    downsweepSequential(Array[Float](0f, 1f, 8f, 15f), output, 0f, 1, 3 )
    assert(output.toList == List(0f, 1f, 4f, 0f))
  }
  test("threashhod1") {
    val output = new Array[Float](2)
    parLineOfSight(Array[Float]( 0f, 9f), output,1)
    assert(output.toList == List(0f, 9f))
  }

  test("test 10000") {
    val length = 10000000
    val input = (0 until length).map(_ % 100 * 1.0f).toArray
    val output1 = new Array[Float](length + 1)
    val output2 = new Array[Float](length + 1)

    lineOfSight(input, output1)




      parLineOfSight(input, output2, 1)

    assert(output1.toList == output2.toList)
  }

}

