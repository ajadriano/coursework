import java.io._
import scala.io.Source

class Coder (keyword: String) {
    val alphabet = "abcdefghijklmnopqrstuvwxyz"
    val characterFilter = (x: Char) => alphabet.contains(x)
    val codeBlock = generateCodeBlock(keyword)
  
  def generateCodeBlock(keyword: String) =
  {
    var characters = keyword.toLowerCase.filter(characterFilter).concat(alphabet).replace('j', 'i').toList.distinct
    (for (i <- 0 until 5) 
      yield Array(characters(i*5),characters(i*5+1),characters(i*5+2),characters(i*5+3),characters(i*5+4))
    ).toArray
  }
    
    def adjustDoubleLetters(input: String) =
  {
    var transformedInput = input.toLowerCase.filter("abcdefghijklmnopqrstuvwxyz".contains(_))
    
     var currentIndex = 0
      
     var list = for {
      j <- 0 until transformedInput.length - 1
      currentChar = transformedInput(j)
      nextChar = transformedInput(j+1)
    } yield 
    (if (currentIndex % 2 == 0 && currentChar == 'x' && nextChar == 'x')
      {
        currentIndex += 2
        Array(currentChar, 'q')
      }
    else if (currentIndex % 2 == 0 && currentChar == nextChar)
      {
        currentIndex += 2
        Array(currentChar, 'x')
      }
     else 
     {
       currentIndex += 1
       Array (currentChar)
     }
    )
    var output = (list.toList ::: List(Array(transformedInput.last))).flatten
    
    if (output.length % 2 != 0)
      output ::: List('z')
      else
        output
  }
    
   def decryptDoubleLetters(input : List[Tuple2[Char, Char]]) =
  {
    var hasZ = input.last._2 == 'z'
    var length = input.length - 1
    var output = (for{
      j <- 0 until input.length
      currentTuple = input(j)
    } yield
    (currentTuple match{
      case Tuple2('x', 'q') => 
        (
            if (j == input.length - 1)
              Array('x', 'q')
            else if (input(j+1)._1 == 'x')
              Array('x')
            else
              Array('x', 'q')
        )
      case Tuple2(a, 'x') => 
        (
            if (j == input.length - 1)
              Array('a', 'x')
            else if (input(j+1)._1 == a)
              Array(a)
            else
              Array(a, 'x')
        )
      case Tuple2(a, b) => Array(a,b)
    })).flatten.toList
    
    if (hasZ) output.slice(0, output.length - 1) else output
  }
  
  def transformPlainText(input: String) =
  {
    var transformedInput = adjustDoubleLetters(input)
    
    (for (i <- 0 until transformedInput.length by 2)
        yield Tuple2(transformedInput(i), transformedInput(i+1))).toList
  }
  
  def transformSecretText(input: String) = 
  {
    var transformedInput = input.toLowerCase.filter(characterFilter)
    require(transformedInput.length > 0, "No characters in secret text.")
    require(transformedInput.length % 2 == 0, "Should have even number of characters.")
    (for (i <- 0 until transformedInput.length by 2)
        yield Tuple2(transformedInput(i), transformedInput(i+1))).toList
  }
  
  def getLocation(codeBlock : Array[Array[Char]], letter: Char) =
  {
      var column = -1
    var row = codeBlock.indexWhere(x => 
      {
        column = x.indexOf(letter)
        column
      } != -1)
    Tuple2(row, column)
  }
  
  def encryptTuple(codeBlock : Array[Array[Char]], tuple: Tuple2[Char, Char]) =
  {
      var location1 = getLocation(codeBlock, tuple._1)
      var location2 = getLocation(codeBlock, tuple._2)
      Tuple2(location1._1 == location2._1, location1._2 == location2._2) match {
        case Tuple2(false, false) => Tuple2(codeBlock(location1._1)(location2._2), codeBlock(location2._1)(location1._2))
        case Tuple2(false, true) => Tuple2(codeBlock((location1._1 + 1) % 5)(location1._2), codeBlock((location2._1 + 1) % 5)(location2._2))
        case Tuple2(true, false) => Tuple2(codeBlock(location1._1)((location1._2 + 1) % 5), codeBlock(location2._1)((location2._2 + 1) % 5))
        case Tuple2(true, true) => throw new IllegalStateException
      }
  }
  
  def decryptTuple(codeBlock : Array[Array[Char]], tuple: Tuple2[Char, Char]) =
  {
      var location1 = getLocation(codeBlock, tuple._1)
      var location2 = getLocation(codeBlock, tuple._2)
      Tuple2(location1._1 == location2._1, location1._2 == location2._2) match {
        case Tuple2(false, false) => Tuple2(codeBlock(location1._1)(location2._2), codeBlock(location2._1)(location1._2))
        case Tuple2(false, true) => Tuple2(codeBlock(if (location1._1 == 0) 4 else location1._1 - 1)(location1._2), codeBlock(if (location2._1 == 0) 4 else location2._1 - 1)(location2._2))
        case Tuple2(true, false) => Tuple2(codeBlock(location1._1)(if (location1._2 == 0) 4 else location1._2 - 1), codeBlock(location2._1)(if (location2._2 == 0) 4 else location2._2 - 1))
        case Tuple2(true, true) => throw new IllegalStateException("same character in pair")
      }
  }
  
  def transformOutput(output : List[Char]) =
  {
    (for (i <- 0 until output.length)
      yield(
        if (i != 0 && i % 50 == 0)
        {
          Array('\n', output(i))
        }
        else if (i != 0 && i % 5 == 0)
        {
          Array(' ', output(i))
        }
        else
        {
          Array(output(i))
        }
      )).flatten.mkString
  }
    
    def encode(plainText: String): String = 
    {
      var transformedInput = transformPlainText(plainText)
    var result = transformedInput.map(encryptTuple(codeBlock, _))
    transformOutput(result.flatMap(x => Array(x._1, x._2)))
    }
    
    def decode(secretText: String): String =
    {
      var transformedSecretText = transformSecretText(secretText)
    var decryptedResultList = transformedSecretText.map(decryptTuple(codeBlock, _))
    var decryptedOutput = decryptDoubleLetters(decryptedResultList)
    transformOutput(decryptedOutput)
    }
}

object MainClass 
{
  def main(args: Array[String]) 
  {
      var running = true
      println(new File(".").getCanonicalPath())
      while (running)
      {
        try
        {
        println("Press e to encode, d to decode, and q to quit")
        print("Option: ")
        var data = readLine()
        if (data.toLowerCase == "e")
        {
          print("Keyword: ")
          var keyword = readLine()
          print("File: ")
          println(new Coder(keyword).encode(Source.fromFile(readLine()).mkString))
        }
        if (data.toLowerCase == "d")
        {
          print("Keyword: ")
          var keyword = readLine()
          print("File: ")
          println(new Coder(keyword).decode(Source.fromFile(readLine()).mkString))
        }
        if (data.toLowerCase == "q")
        {
            println("quitting...")
          running = false
        }
        }
        catch
        {
          case e : Exception => println(e)
        }
      }
  }
}