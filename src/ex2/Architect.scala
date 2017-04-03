package ex2

sealed trait Shape
case class Triangle(a: Int, b: Int, c: Int, h: Int) extends Shape // h represent the height against the longest side of the triangle
case class Rectangle(a: Int, b: Int) extends Shape
case class Trapezoid(a: Int, b: Int, h: Int) extends Shape
case class Cube() extends Shape

class Architect {

  /*
   *  Finds the max element from given list of integers.
   *  The result is wrapped in an Option instance. The Option has two forms:
   *   - None if no element satisfies the search criteria (for example, in case an empty list is provided)
   *   - Some(x), where x if the searched element. In this case, it can be acquired with the method get. Example:
   *     val o: Option[Int] = Some(6)
   *     val n: Int = o.get
   */
  def max(xs: List[Int]): Option[Int] = {
    def iter(xs: List[Int],curMax:Int): Option[Int] = {
      if(xs.isEmpty) Some(curMax) 
      else {
        iter(xs.tail,if (xs.head > curMax) xs.head else curMax);
      }
    }
    
    if(xs.isEmpty) None else iter(xs.tail, xs.head);
    
  }

  // Determines the type of given triangle: "rectangular", "equilateral", "isosceles", "random"
  def triangleType(t: Triangle): String =  {
    if(!isTriangle(t)) return "not";
    
    if (t.a != t.b && t.b != t.c && t.a!=t.c){
      val aSquare = t.a*t.a;
      val bSquare = t.b*t.b;
      val cSquare = t.c*t.c;
      
      // Check for rectangular triangle
      if((aSquare + bSquare == cSquare)
          || (aSquare + cSquare == bSquare) 
          || (bSquare + cSquare == aSquare)) "rectangular" else "random";
               
    }else if(t.a == t.b && t.b == t.c) "equilateral";
     else "isosceles";
    
  }

  /*
   * Calculates the area of the provided shape, by using these formulae:
   *  - Rectangular triangle: a * b / 2, where a and b are cathetus
   *  - Any triangle except rectangular: x * h / 2, where x is the largest side of the triangle and h is the opposite height
   *  - Rectangle: a * b, where a and b are both sides
   *  - Trapezoid: (a + b) * h / 2, where a and b are the parallel sides and h is the height between them
   *  - Cube: always return -1
   * 
   *  Hint: for triangles use the max function
   */
  def area(s: Shape): Double = s match{
    case t:Triangle => triangleType(t) match{
      case "rectangular" => max(List(t.a,t.b,t.c)).get match{
        case t.a => (t.b * t.c) / 2;
        case t.b => (t.a * t.c) / 2;
        case t.c => (t.a * t.b) / 2;
      }
      // Returns -2 if not a triangle
      case "not" => -2;
      case _ => (max(List(t.a,t.b,t.c)).get * t.h) .toDouble / 2;
    }
    case Rectangle(a,b) => a*b;
    case Trapezoid(a,b,h) => (a + b).toDouble * h / 2;
    case Cube() =>  -1;
  }

  /*
   *  Returns the number of rectangular triangles in given list of shapes
   *  
   *  Hint: use the triangleType function
   */
  def findRectangulars(shapes: List[Shape]): Int = {
    def iter(shapes: List[Shape], n: Int): Int = {
      if(shapes.isEmpty) n;
      else iter(shapes.tail,shapes.head match{
        case t:Triangle => if(triangleType(t).equals("rectangular")) n+1 else n;
        case s:Shape => n; 
      });       
    }
    iter(shapes, 0);
  }
  
  def isTriangle(t:Triangle)={
    if(t.a + t.b > t.c && t.b + t.c > t.a && t.a + t.c > t.b) true else false;
  }
}
