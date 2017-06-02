package ex3

/**
  * Created by Ivan Atanassov on 5/2/2017.
  */
object StreamsExercise {

  /**
    * Creates infinite sequence of natural numbers starting from {@code n} .
    * @param n the fist number of the sequence
    * @return infinite sequence of naturals
    */
  def from(n:Int): Stream[Int] = Stream.cons(n, from(n + 1))

  /**
    * Infinite sequence of a, 2*a, 2*2*a,..
    * @param a starting number
    * @return Infinite sequence of a, 2*a, 2*2*a,..
    */
  def multiplyByTwo(a: Int) : Stream[Int] = a #:: multiplyByTwo(2*a)

  /**
    *
    * Represents the sequence of all powers of 2, starting form 2
    */
  lazy val powersOfTwo : Stream[Int] = multiplyByTwo(2)

  /**
    * Defines the sequence of all fibonacci numbers starting with the two
    * defined as parameters.
    * @param fib1 the second fibonacci number
    * @param fib2 the first fibonacci number
    * @return fibonacci numbers' sequence
    */
  def fibFrom(fib1: Int, fib2: Int): Stream[Int] = fib1 #:: fibFrom(fib2,  fib1 + fib2)

  /**
    * The sequence of all fibonacci numbers starting from 1
    */
  lazy val fibs = fibFrom(1, 1)

  /**
    * Returns the biggest number of specified digits.
    * @example 99 is the biggest number of 2 digits and 999 is the biggest with 3 digits.
    * @param digits number of digits
    * @return the biggest number with <code>n</code> digits
    */
  def maxFixedDigitsNumber(digits: Int) = math.pow(10, digits).toInt - 1

  /**
    * Calculates the sum of digits of <code>number</code>
    * @param number natural number
    * @return the sum of the digits of <code>number</code>
    * @see Don't do it with 'for' expressions. Instead transform the number to it's
    *      string representations and use the specifics that String in Scala is kind of
    *      sequence and has methods as 'map', etc.
    * @see Know as a hint for the implementation that every char representing a digit
    *      has "asDigit" method to get its value
    */
  def sumOfDigits(number: Int) = number.toString.map(_.asDigit).sum

  /**
    * Calculates the sum of digits powered by 3 of <code>number</code>
    * @param number natural number
    * @return the sum of the digits powered by 3 of <code>number</code>
    * @example sumOfDigitsPowThree(153) = 1*1*1 + 5*5*5 + 3*3*3 = 153
    * @see Don't do it with 'for' expressions. Instead transform the number to it's
    *      string representations and use the specifics that String in Scala is kind of
    *      sequence and has methods as 'map', etc.
    * @see Know as a hint for the implementation that every char representing a digit
    *      has "asDigit" method to get its value
    */
  def sumOfDigitsPowThree(number: Int) =  number.toString.map(_.asDigit).map(math.pow(_, 3).toInt).sum

  /**
    * TASK1 - find all natural numbers which sum of the third powers of the their digits
    * is equal to the number itself.
    * @example 153 = 1*1*1 + 5*5*5 + 3*3*3
    */
  lazy val numEqualsPowerOfThree = from(0).filter(el => el == sumOfDigitsPowThree(el));

  /**
    * With the growth of the number's number of digits its value grows by 10 and the sum of third powers
    * if its digits grows with maximum 9^3=729. So with the growth of the digits number, the numbers grow
    * much faster than the sum of the third power of the digits and at sometime it would be impossible
    * for the numbers to satisfy TASK1 condition (defined above).
    *
    * Find which is the maximum sizeof digits where it's still possible to have number with such digit
    * size satisfying the TASK1 condition.
    * @example for numbers of 6 digits theirs sum of the third powers of the digits is maximum 6x(9^3),
    *          which is 4374 and it's number with only 4 digits, much less that the minimum number of
    *          6 digits: 100000.
    * @see do not use 'for' operators, implement it based on the streams of all natural numbers and
    *      find the desired number based on that stream.
    */
  val capOfDigitsSizeTask1 = from(1).take(10).toList.filter(x => math.pow(10, x) < x * math.pow(9,3) + 1).max + 1;

  /**
    * The stream of all numbers which may satisfy the TASK1 (defined above) conditions
    * @see Actually it is a stream computed from a range from 1 to the maximum number of
    *      the calculated capOfDigitsSizeTask1 digits.
    */
  lazy val rangeStreamTask1 = (1 to capOfDigitsSizeTask1 * math.pow(9, 3).toInt).toStream;

  /**
    * The stream of all numbers satisfying Task1 condition.
    */
  lazy val resultStreamTask1 = rangeStreamTask1.filter(el => el == sumOfDigitsPowThree(el))


  def main(args: Array[String]): Unit = {
    println("The fifth fibonacci number is " + fibFrom(1,1)(4))

    println("Task1 numbers are " + resultStreamTask1.toList)
    // the expected result is List(1, 153, 370, 371, 407)

  }
}
