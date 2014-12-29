//Class Demo 
//CS345H Jan Rellermeyer
//12-3-14
object Main extends ScIntr {

  def main(args: Array[String]):Unit = {
    try{
      
    /*canceling and aliasing*/
    DEFINE ("mi" := "kilo-ampheres" ^ 3)
    DEFINE ("hotdog" := "mi*moles")
    'a := 10 ("hotdog")
    'p := 1 ("1/moles*kilo-ampheres")
    's := 'a * 'p
    PRINT('s)
    'b := 5 ("mi*candelas*moles*moles")
    PRINT('b)
    'd := 1 ("kilo-ampheres*moles")
    'c := 'b / 'd
    PRINT('c)
    
    /*right associativity*/
    DEFINE("oz" := "grams" SCALE 28)
    'a := 3("oz")
    'b := 56("grams")
    'c := 'a + 'b
    'd := 'b + 'a
    PRINT('c)
    PRINT('d)
    'c TO "grams"
    PRINT('c)
    
    /*compatibility checks*/
    DEFINE("mi" := "kilo-meters" SCALE 1.6)
    'a := 16 ("kilo-meters")
    'b := 10 ("mi")
    'c := 12 ("mi")
    println('a =:= 'b)
    println('a === 'b)
    println('c === 'b)
    println('c =:= 'b)
    println('c =:= "kilo-meters")
    
    /*error checking*/
    'a := 2 ("oz")
    'b := 5 ("mi")
    'c := 'a + 'b
    }
    catch{
      case ex:SciException => println(ex.getMessage())
    }
  }

}