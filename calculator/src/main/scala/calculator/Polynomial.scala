package calculator

object Polynomial {
  def computeDelta(a: Signal[Double], b: Signal[Double],
      c: Signal[Double]): Signal[Double] = {

    Signal{
      val a_value=a()
      val b_value=b()
      val c_value=c()
      b_value*b_value-4*a_value*c_value
    }
  }

  def computeSolutions(a: Signal[Double], b: Signal[Double],
      c: Signal[Double], delta: Signal[Double]): Signal[Set[Double]] = {


    Signal{
      val a_value=a()
      val b_value=b()
      val c_value=c()
      val d_value=delta()
      if (d_value<0) Set()
      else if (d_value==0) Set(-b_value/(2*a_value))
      else Set((-b_value+Math.sqrt(d_value))/(2*a_value),(-b_value-Math.sqrt(d_value))/(2*a_value))
    }
  }
}
