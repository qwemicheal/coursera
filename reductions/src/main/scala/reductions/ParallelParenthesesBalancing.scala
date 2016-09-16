package reductions

import scala.annotation._
import org.scalameter._
import common._

object ParallelParenthesesBalancingRunner {

  @volatile var seqResult = false

  @volatile var parResult = false

  val standardConfig = config(
    Key.exec.minWarmupRuns -> 40,
    Key.exec.maxWarmupRuns -> 80,
    Key.exec.benchRuns -> 120,
    Key.verbose -> true
  ) withWarmer(new Warmer.Default)

  def main(args: Array[String]): Unit = {
    val length = 100000000
    val chars = new Array[Char](length)
    val threshold = 10000
    val seqtime = standardConfig measure {
      seqResult = ParallelParenthesesBalancing.balance(chars)
    }
    println(s"sequential result = $seqResult")
    println(s"sequential balancing time: $seqtime ms")

    val fjtime = standardConfig measure {
      parResult = ParallelParenthesesBalancing.parBalance(chars, threshold)
    }
    println(s"parallel result = $parResult")
    println(s"parallel balancing time: $fjtime ms")
    println(s"speedup: ${seqtime / fjtime}")
  }
}

object ParallelParenthesesBalancing {

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def balance(chars: Array[Char]): Boolean = {
    def check(char_left: Array[Char],re:Int):Boolean={
      if (re<0) false
      else{
      if (char_left.isEmpty) {
        if (re==0) true
        else false
      }else
      if (char_left.head=='(') check(char_left.tail,re+1)
      else if (char_left.head==')') check(char_left.tail,re-1)
      else check(char_left.tail,re)
    }
    }
    check(chars,0)
  }

  /** Returns `true` iff the parentheses in the input `chars` are balanced.
   */
  def parBalance(chars: Array[Char], threshold: Int): Boolean = {
    //arg1 used for
    def traverse(idx: Int, until: Int, arg1: Int, arg2: Int) :Int = {
      if (arg2==1){
        if ((chars(idx)==')')&&(arg1-1<0)) -chars.length
        else
        if (idx == until - 1)
          chars(idx) match {
            case '(' => 1 + arg1
            case ')' => arg1 - 1
            case _ => arg1
          }
        else chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' => traverse(idx + 1, until, arg1 - 1, arg2)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }else {
        if (idx == until - 1)
          chars(idx) match {
            case '(' => 1 + arg1
            case ')' => arg1 - 1
            case _ => arg1
          }
        else chars(idx) match {
          case '(' => traverse(idx + 1, until, arg1 + 1, arg2)
          case ')' => traverse(idx + 1, until, arg1 - 1, arg2)
          case _ => traverse(idx + 1, until, arg1, arg2)
        }
      }
    }

    def reduce(from: Int, until: Int) :Int = {
      if (until==0) 0
      else
      if ((until-from)<=threshold) {
        if (from == 0)
          traverse(from, until, 0, 1)
        else
          traverse(from, until, 0, 0)
      }
      else {
        val mid=from+(until-from)/2
        val (a,b)=parallel(reduce(from,mid),reduce(mid, until ))
        a+b

    }
    }

    reduce(0, chars.length) == 0
  }

  // For those who want more:
  // Prove that your reduction operator is associative!

}
