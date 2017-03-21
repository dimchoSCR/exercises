package ex1

object Functions {

  // Връща големината на масив (без да ползва data.length!!!)
  def length(data: List[Int]): Int = {   
    def lengthMain(data: List[Int], acc: Int): Int = {
      if (data.isEmpty) acc;
      else lengthMain(data.tail, acc + 1);
    }
    
    lengthMain(data, 0);
  } 

  // Ако cond е true връща onTrue
 def ifelse(cond: Boolean, onTrue: => Int, onFalse: => Int) = {
    if (cond) onTrue else onFalse;
 }

  // Проверява дали скобите в даден масив от символи са балансирани.
  // Коректно: (a)asda(b)(v) | (((a))) | ()(()асдасд)
  // Грешно: )() | ((д) | ((das) (d)( 
  def balance(chars: List[Char]):Boolean={
    def balanceMain(chars: List[Char],openningBracketsCnt:Int,closingBracketsCnt:Int): Boolean = {
      // When the closingBrackectsCnt becomes grater than the openningBracketsCnt
      // no corresponding opening bracket could be added so the result is false
      if(closingBracketsCnt > openningBracketsCnt){
        return false;
      }
      
      if(chars.isEmpty)
        // If the number of opening brackets match the number of closing brackets then
        // the brackets are balanced
        // otherwise they are not balanced
        if(openningBracketsCnt == closingBracketsCnt)true else false;
      // Iterates through the characters
      else { 
       // Count the ')' and move to the next char
       if(chars.head == ')') 
         balanceMain(chars.tail, openningBracketsCnt, closingBracketsCnt+1); 
       else if(chars.head == '(')
         balanceMain(chars.tail, openningBracketsCnt+1, closingBracketsCnt); 
       // Skip any letters and continue looking for '(' or ')'
       else balanceMain(chars.tail, openningBracketsCnt, closingBracketsCnt); 
      }
    }
    
    // Needed because of the next if expression (optimization)
    if(chars.isEmpty){
      return true;
    }
    
    // If a character list starts directly with ')' the brackets are not balanced
    if(chars.head == ')') false;
    else balanceMain(chars, 0, 0);
  }

  def map(chars: List[Char], f: (Char) => Any)={
    def innerMap(chars: List[Char], f: (Char) => Any, mapped: List[Any]): List[Any] = {
      if (chars.isEmpty) mapped;
      else {
        // Appends the processed by f chars
        innerMap(chars.tail, f, mapped :+ f(chars.head));
      }
    }
    innerMap(chars, f, List());
  }

  def toUpperCase(chars: List[Char]) = {
    def upperCase(char: Char) ={
      // Check to see if the char is a lower case letter
      if(char >= 97 && char<=122) (char - 32).toChar;
      else "Not a lowercase letter";
    }
    
    // Map all lower case chars to upper case chars using map
    map(chars, upperCase);
  }

  // Проверява дали съществува елемент отговарящ на f
  def exists(data: List[Int], f: (Int) => Boolean): Boolean = {
    if (data.isEmpty) false;
    else if (f(data.head)) true; else exists(data.tail, f);
  }

  // Връща масив съдържащ само елементите отговарящи на f
  def filter(data: List[Int], f :(Int) => Boolean) = {
    def innerFilter(data: List[Int], f :(Int) => Boolean,filtered:Array[Int]):Array[Int] = {
      if(data.isEmpty) filtered;
      else{
        // If the criteria is satisfied the head is inserted into the array 
        if(f(data.head)) innerFilter(data.tail, f, filtered :+ data.head)
        // Continue iterating over data
        else innerFilter(data.tail,f,filtered);
      }
    }
    
    innerFilter(data, f, Array());
  }

  // Проверява дали всички елементи отговарят на f
  def forall(data: List[Int], f: (Int) => Boolean): Boolean = {
    !exists(data, !f(_));
  }

  // Връща числото от триъгълника на Паскал отговарящо на съответния ред/колона
  def pascal(c:Int,r:Int):Int = {
    def pascalInner(c: Int, r: Int): Int = {   
      if(c == 0 || c == r) 1;
      else pascalInner(c-1, r-1) + pascalInner(c,r-1);
    }
    
    // Performing argument validations outside the recursive function
    // Check for invalid column numbers and negative rows and columns
    if(c>r || (c<0 || r<0)){
      return -1;
    }
   
    pascalInner(c, r);
  }
}
