import scala.annotation.tailrec

class Employee(
    val firstName: String, 
    val lastName: String, 
    val experience: Int, 
    val salary: Double)
    
case class Manager(
    fName: String, 
    lName: String, 
    exp: Int, 
    sal: Double, 
    managedEmpCount: Int, 
    bonus: Double)
extends Employee(fName, lName, exp, sal)

object Exam1 {
  def main(args: Array[String]): Unit = {
    val emp1 = new Employee("Иван", "Иванов", 3, 12000)
    val emp2 = new Manager("Петър", "Петров", 12, 20000, 8, 2000)
    val emp3 = new Employee("Георги", "Георгиев", 5, 15000)
    val emp4 = new Employee("Страхил", "Стаматов", 35, 7000)
    val emp5 = new Manager("Десислава", "Тодорова", 7, 18000, 7, 3000)

    val employees = List(emp1, emp2, emp3, emp4, emp5)

    // 1A. Печати годишното заплащане на един служител и един мениджър
    println("============= 1A =============")
    println(getYearPayment(emp1))
    println(getYearPayment(emp2))

    // 1Б. Връща променена заплата на един служител с 10% и 500лв
    println("============= 1Б =============")
    println(updatePayment(emp1, x => x * 0.10 + x))
    println(updatePayment(emp1, x => x + 500))
    
    println("============= 2A =============")
    firstAndLastNameStartWithSameLetter(employees)

    println("============= 2Б =============")
    println(totalPaymentPerYear(employees))
    
    println("============= 2B =============")
    println(getSalaryIncrease(employees))
    
    println("============= 3 =============")
    // Tail rec
    println(sequence(1, 5, x => x * 2))
    println(sequence(7, 12, x => x * x))
    
    // No tail rec
    println(sequenceNoTailRec(1, 5, x => x * 2))
    println(sequenceNoTailRec(7, 12, x => x * x))
  }

  /** 1A
   *  Връща годишното заплащане според типа на служителя
   */
  def getYearPayment(e: Employee) = e match {
    case m: Manager  => m.salary + m.bonus
    case e: Employee => e.salary
  }

  /**
   * 1Б. Прилага функция от по-висок ред върху заплатата на служителя и 
   * връща резултата.
   */
  def updatePayment(e: Employee, f: Double => Double) = f(e.salary)

  /**
   * 2A. Извежда първите име на служителите, чийто първо име и фамилия
   * започват с една и съща буква.
   */
  def firstAndLastNameStartWithSameLetter(data: List[Employee]) = 
    data.filter(x => x.firstName.charAt(0) == x.lastName.charAt(0))
        .map(x => x.firstName)
        .foreach(println)

  /**
   * 2Б. Връща общата сума, която фирмата трябва да плати на годишна база
   * на служителите си.
   */
  def totalPaymentPerYear(data: List[Employee]) =
    data.map(getYearPayment).reduce(_ + _)

  /**
   * 2В. Връща сумата пари, която би доплатила фирмата (на годишна база), 
   * ако увелича заплатата на служителите с 2% за всеки 5 прослужени години.
   */
  def getSalaryIncrease(data: List[Employee]) =
    data.map(x => ((x.experience / 5) * 0.02) * x.salary).reduce(_ + _)

  /**
   * 3. Връща редица с числа в даден интервал, чиято стойност е 
   * равна на приложена функция в/у съответния индекс. Изпозлва
   * tailrec оптимизация (линеен итеративен процес) 
   */
  def sequence(from: Int, to: Int, f: Int => Int) = {
    @tailrec
    def loop(idx: Int, result: List[Int]): List[Int] = 
      if (idx < from) result
      else loop(idx - 1, f(idx) :: result)

    loop(to, Nil)
  }

  /**
   * 3. Алтернативно решение на 3 използащо линеен рекурсивен процес.
   * !!! За изпита е достатъчно едно от двете решения !!!
   */
  def sequenceNoTailRec(from: Int, to: Int, f: Int => Int): List[Int] = {
    if (from > to) Nil
    else f(from) :: sequenceNoTailRec(from + 1, to, f)
  }
}
