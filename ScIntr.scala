import scala.collection.mutable
class ScIntr {
  /*Hash map for variables*/
  val svars = new mutable.HashMap[Symbol, SVal]
  
  /*Hash map for aliases*/
  val alias = new mutable.HashMap[String, SEntry]
  
  /*Exception definitions*/
  class SciException(message:String) extends Exception(message)
  case class IllegalUnitFormatException(message:String) extends SciException(message)
  case class IncompatiableArithmaticException(message:String) extends SciException(message)
  case class IllegalPrintFormatException(message:String) extends SciException(message)
  case class IllegalConversionException(message:String) extends SciException(message)
  case class IllegalComparisonException(message:String) extends SciException(message)
  case class IllegalAliasException(message:String) extends SciException(message)
  
  /*Call to define alias*/
  object DEFINE{
    def apply(entry:SEntry) = {
      alias(entry.uu.str.top) = entry
    }
  }
  
  /*Returns the (user scaled) value associated with a symbol*/
  object VAL{
    def apply(sym:Symbol):Double = svars(sym).value/svars(sym).entry.uu.scale 
  }
  
  /*Return a string representation of the symbol*/
  object STRING{
    def apply(sym:Symbol):String = {val sval:SVal = svars(sym); (sval.value/sval.entry.uu.scale + " " + sval.entry.uu.str)}
  }
  
  object PRINT{
    def apply(sym:Symbol) = {val sval:SVal = svars(sym); println(sval.value/sval.entry.uu.scale + " " + sval.entry.uu.str)}
    def apply(str:String) = println(str)
  }
  
  case class Converter(sym:Symbol) {
    def TO(str:String) = {
      val entry = parse_units(str)
      if(entry.u != svars(sym).entry.u)
        throw new IllegalConversionException("ERROR: symbol "+sym+" of type "+svars(sym).entry.uu.str+" cannot be converted to type "+entry.uu.str)
      svars(sym).entry = entry 
    }
  }
  
  case class Assignment(sym:Symbol) {
      def :=(sval:SVal):Unit = svars(sym) = sval
      def :=(fn:() => SVal):Unit = {
          svars(sym) = fn() 
      }
      def :=(i:Double):Unit = svars(sym) = SVal(i,SEntry(SUnit(0,0,0,0,0,0,0),Scalar()))
    }
  
  case class Comparison(sym:Symbol) {
      /*checks for type compatibility*/
	  def =:=(rhs:Symbol):Boolean = svars(sym).entry.u == svars(rhs).entry.u
	  /*works for strings as well*/
	  def =:=(rhs:String):Boolean = svars(sym).entry.u == parse_units(rhs).u 
	  /*checks equality and compatibility*/
	  def ===(rhs:Symbol):Boolean = (svars(sym).entry.u == svars(rhs).entry.u) && (svars(sym).value == svars(rhs).value)
	  /*checks equality and throws exception if types are incompatible*/
	  def ==(rhs:Symbol):Boolean = {
	    if(svars(sym).entry.u != svars(rhs).entry.u)
	      throw new IllegalComparisonException("ERROR: symbols "+sym+" and "+rhs+" are not of comparable types")
	    svars(sym).value == svars(rhs).value 
	  }
	  def <=(rhs:Symbol):Boolean = {
	    if(svars(sym).entry.u != svars(rhs).entry.u)
	      throw new IllegalComparisonException("ERROR: symbols "+sym+" and "+rhs+" are not of comparable types")
	   svars(sym).value <= svars(rhs).value 
	  }
	  def >=(rhs:Symbol):Boolean = {
	    if(svars(sym).entry.u != svars(rhs).entry.u)
	      throw new IllegalComparisonException("ERROR: symbols "+sym+" and "+rhs+" are not of comparable types")
	    svars(sym).value >= svars(rhs).value 
	  }
	  def >(rhs:Symbol):Boolean = {
	    if(svars(sym).entry.u != svars(rhs).entry.u)
	      throw new IllegalComparisonException("ERROR: symbols "+sym+" and "+rhs+" are not of comparable types")
	    svars(sym).value > svars(rhs).value 
	  }
	  def <(rhs:Symbol):Boolean = {
	    if(svars(sym).entry.u != svars(rhs).entry.u)
	      throw new IllegalComparisonException("ERROR: symbols "+sym+" and "+rhs+" are not of comparable types")
	    svars(sym).value < svars(rhs).value 
	  }
	  
  }
  
  /*Alias strings cannot contain '*' or '/'*/
  case class Alias(s:String){
      def :=(entry:SEntry):SEntry = {
            if(s.contains("*")||s.contains("/"))
            	throw new IllegalAliasException("ERROR: Alias "+s+" contains illegal characters")
            entry.uu.str = UnitString(s,""); 
            entry
        }
      def :=(unit:String):SEntry = {
            if(s.contains("*")||s.contains("/"))
            	throw new IllegalAliasException("ERROR: Alias "+s+" contains illegal characters")
	        val entry = parse_units(unit); 
	        entry.uu.str = UnitString(s,""); 
	        entry
        }
  }
  
  /*Class used when scaling aliases*/
  case class aliasUnitBuilder(str:String){
      def SCALE(i:Double): SEntry = {
          val entry:SEntry = parse_units(str)
          entry.uu.scale = entry.uu.scale*i 
          entry
      }
      def ^(i:Integer): SEntry = {
          var lookup:String = ((str+"*")*(Math.abs(i)-1))+str
          if(i < 0)
            lookup = "1/" + lookup
          parse_units(lookup)
      }
  }
  
  /*Used to create SVal from value and units*/
  case class unitBuilder(i:Double){
      def apply(v:String):SVal = {
	        val entry:SEntry  = parse_units(v)
	        SVal(i*entry.uu.scale, entry )
       
      }
  }
  
  /*Allows printing in user units*/
  case class printBuilder(sym:Symbol){
      def AS(str:String):String ={
        val format:SEntry = parse_units(str)
        if(svars(sym).entry.u != format.u ) {
          throw new IllegalPrintFormatException("ERROR: symbol "+sym+" of type "+svars(sym).entry.uu.str+" cannot be printed as "+str)
        }
        (svars(sym).value/(format.uu.scale)).toString
      }
  }
  
    case class MathFunction(lhs:() => SVal) {
      def *(rhs:Symbol):() => SVal = () => lhs() * svars(rhs)
      def *(rhs:() => SVal):() => SVal = () => lhs() * rhs()
      def /(rhs:Symbol):() => SVal = () => lhs() / svars(rhs)
      def /(rhs:() => SVal):() => SVal = () => lhs() / rhs()
      def +(rhs:Symbol):() => SVal = () => lhs() + svars(rhs)
      def +(rhs:() => SVal):() => SVal = () => lhs() + rhs()
      def -(rhs:Symbol):() => SVal = () => lhs() - svars(rhs)
      def -(rhs:() => SVal):() => SVal = () => lhs() - rhs()
    }
  
  /*Case class for fundamental units. An array representation would have been more elegant...*/  
  case class SUnit(g:Int,s:Int,m:Int,mol:Int,k:Int,a:Int,cd:Int){
    def +(rhs:SUnit):SUnit = SUnit(g + rhs.g, s + rhs.s, m + rhs.m, mol + rhs.mol, k + rhs.k, a + rhs.a , cd + rhs.cd)
    def -(rhs:SUnit):SUnit = SUnit(g - rhs.g, s - rhs.s, m - rhs.m, mol - rhs.mol, k - rhs.k, a - rhs.a , cd - rhs.cd)
    def ==(rhs:SUnit):Boolean = ((g == rhs.g) && (s == rhs.s) && (m == rhs.m) && (mol == rhs.mol) && (k == rhs.k) && (a == rhs.a) && (cd == rhs.cd))
    def !=(rhs:SUnit):Boolean = !((g == rhs.g) && (s == rhs.s) && (m == rhs.m) && (mol == rhs.mol) && (k == rhs.k) && (a == rhs.a) && (cd == rhs.cd))

  }
  
  /*Value associated with a symbol*/
  case class SVal(value:Double,var entry:SEntry){
    def +(rhs:SVal):SVal = {
      if(entry.u != rhs.entry.u ){
        throw new IncompatiableArithmaticException("ERROR: types "+entry.uu.str + " and " + rhs.entry.uu.str+" are not compatible for addition and subtraction")
      }
      SVal(value + rhs.value,entry)
    }
    def -(rhs:SVal):SVal = {
      if(entry.u != rhs.entry.u ){
        throw new IncompatiableArithmaticException("ERROR: types "+entry.uu.str + " and " + rhs.entry.uu.str+" are not compatible for addition and subtraction")
      }
      SVal(value - rhs.value,entry)
    }
    def *(rhs:SVal):SVal = {
      var base:UserUnits = entry.uu 
      var cancel:UserUnits = rhs.entry.uu
      val new_units:ReducedUnits = cancel_units(base,cancel,false)
      if(!new_units.nul){
        correctUU(new_units.base)
        correctUU(new_units.cancel) 
        base = new_units.base 
        cancel = new_units.cancel
      }
      SVal(value*rhs.value  , SEntry(entry.u + rhs.entry.u, base + cancel ))
    }
    def /(rhs:SVal):SVal = {
      var base:UserUnits = entry.uu 
      var cancel:UserUnits = rhs.entry.uu
      val new_units:ReducedUnits = cancel_units(base,cancel,true)
      if(!new_units.nul){
        correctUU(new_units.base)
        correctUU(new_units.cancel) 
        base = new_units.base 
        cancel = new_units.cancel
      }
      SVal(value/rhs.value  , SEntry(entry.u - rhs.entry.u, base - cancel ))
    }
  }
  
  /*Used to encapsulate the return from cancel_units*/
  case class ReducedUnits(base:UserUnits,cancel:UserUnits){
    def nul():Boolean = (cancel.isNull && base.isNull)
  }
  
  /*Encapsulates fundamental and user units*/
  case class SEntry(u:SUnit,uu:UserUnits)
  
  /*Fundamental units + a scale. Used to encapsulate the return from get_unit_scale*/
  case class SUnitScale(scale:Double,unit:SUnit){
    def +(rhs:SUnitScale):SUnitScale = SUnitScale(scale*rhs.scale,unit + rhs.unit )
    def -(rhs:SUnitScale):SUnitScale = SUnitScale(scale/rhs.scale,unit - rhs.unit )
  }
  
  trait UserUnits{
    var str:UnitString 
    var scale:Double
    var P:UserUnits
    def +(rhs:UserUnits):UserUnits = rhs match{
      case Null() => this
      case Scalar() => this
      case _ => {
        this match{
          case Null() => rhs
          case Scalar() => rhs
          case _ => val ret = Mul(str*rhs.str,scale*rhs.scale,this,rhs,Scalar()); this.P = ret; rhs.P = ret; ret
        }
        }
    }
    def -(rhs:UserUnits):UserUnits = rhs match{
      case Null() => this
      case Scalar() => this
      case _ => {val ret = Div(str/rhs.str,scale/rhs.scale,this,rhs,Scalar()); this.P = ret; rhs.P = ret; ret}
    }
    def ==(rhs:UserUnits):Boolean = ((rhs.str == str) && (scale == rhs.scale))
    def isNull():Boolean = scale == 0
  }
  
  trait Op extends UserUnits{
    var L:UserUnits
    var R:UserUnits
  }
  
  /*String representation of a user unit.*/
  /*Needed to ensure that the print units are consistent with the user units format*/
  case class UnitString(top:String,bottom:String){
    def *(rhs:UnitString):UnitString = {
      var ntop:String = ""
      var nbot:String = ""
      if(top != ""){
        ntop = top
        if(rhs.top != "")
          ntop = ntop+"*"
      }
      if(bottom != ""){
        nbot = bottom
        if(rhs.bottom != "")
          nbot = nbot+"*"
      }
      UnitString(ntop+rhs.top, nbot+rhs.bottom )
    }
    def /(rhs:UnitString):UnitString = {
      var ntop:String = ""
      var nbot:String = ""
      if(top != ""){
        ntop = top
        if(rhs.bottom != "")
          ntop = ntop+"*"
      }
      if(bottom != ""){
        nbot = bottom
        if(rhs.top != "")
          nbot = nbot+"*"
      }
      UnitString(ntop+rhs.bottom, nbot+rhs.top)
    }
    
    def ==(rhs:UnitString):Boolean = ((top == rhs.top) && (bottom == rhs.bottom))
    
    override def toString() = {
      if(bottom == ""){
        if(top == "") "SCALAR" else top
      }
      else{
        if(top == "") "1/"+bottom else top + "/" + bottom
      }
    }
     
  }
  
  /*Used to represent User Unit nodes*/
  case class Leaf(override var str:UnitString,override var scale:Double, override var P:UserUnits, fun:SUnit) extends UserUnits
  case class Mul(override var str:UnitString, override var scale:Double, override var L:UserUnits, override var R:UserUnits, override var P:UserUnits) extends Op
  case class Div(override var str:UnitString, override var scale:Double, override var L:UserUnits, override var R:UserUnits, override var P:UserUnits) extends Op
  case class Scalar() extends UserUnits {
    var str:UnitString = UnitString("","")
    var scale = 1.0
    var P:UserUnits = this
  } 
  /*reserved for errors/no return*/
  case class Null() extends UserUnits {
    var str:UnitString = UnitString("","")
    var scale = 0.0
    var P:UserUnits = this
  } 

  private def parse_units(str:String): SEntry =  {
    var unit = SUnit(0,0,0,0,0,0,0)
    var top:UserUnits = Null()
    var bottom:UserUnits = Null()
    var result:UserUnits = Null()
    var temp:UserUnits = Null()
    val split = str.split("/")
    if(split(0) == "1"){
      top = Scalar()
    }
    else{
	    split(0).split("\\*").foreach(f => {
	      val ns = get_unit_scale(f)
	      unit = unit + ns.unit
	      temp = if(alias.contains(f)) copyUU(alias(f).uu) else Leaf(UnitString(f,""),ns.scale,Scalar(),ns.unit)
	      top = if(!top.isNull) top + temp else temp
	      })
    }
    if(split.length == 2){
       split(1).split("\\*").foreach(f => {
    	  val ns = get_unit_scale(f)
	      unit = unit - ns.unit
	      temp = if(alias.contains(f)) copyUU(alias(f).uu) else Leaf(UnitString(f,""),ns.scale,Scalar(),ns.unit)
	      bottom = if(!bottom.isNull) bottom + temp else temp 
      })
      result = top - bottom
    }
    else {
      result = top
    }
    SEntry(unit ,result)
  }
  
  /*Attempts to reduce two user units*/
  /*Null indicates it was not possible to reduce the trees*/
  /*Otherwise, the reduced pair is returned*/
    private def cancel_units(base:UserUnits,cancel:UserUnits, del:Boolean):ReducedUnits = cancel match{
    case Null() =>   ReducedUnits(Null(),Null())
    case Scalar() => ReducedUnits(Null(),Null())
    case Leaf(_,_,_,_) => {
      val nbase = cancelLeaf(base,cancel.asInstanceOf[Leaf],del)
      if(!nbase.isNull)  ReducedUnits(nbase,trim_tree(cancel))
      else{
    	  ReducedUnits(Null(),Null())
      }
      }
    case Mul(_,_,l:UserUnits,r:UserUnits,_) => {
      val l_result = cancel_units(base,l,del) 
      val r_result = if (l_result.nul) cancel_units(base,r,del) else cancel_units(l_result.base ,r,del)
      if  (r_result.nul) l_result else r_result
      }
    case Div(_,_,l:UserUnits,r:UserUnits,_) => {
      val l_result = cancel_units(base,l,del) 
      val r_result = if (l_result.nul) cancel_units(base,r,!del) else cancel_units(l_result.base ,r,!del)
      if  (r_result.nul) l_result else r_result
      }
  }
  
  /*Attempts to cancel a particular leaf*/
  /*Null indicates it was not possible*/
  /*Otherwise, the new user units are returned*/
  private def cancelLeaf(tree:UserUnits, leaf:Leaf, del:Boolean):UserUnits = tree match{
    case Null() => Null()
    case Scalar() => Null()
    case Leaf(str,scale,p:UserUnits,fund:SUnit) => {
    	if(!((fund == leaf.fun) && del)){
    	  return Null()
    	}
    	else {
    	  return trim_tree(tree)
    	}
    }
    case oP:Op => {
        var ret:UserUnits = cancelLeaf(oP.L,leaf,del)
        if(!ret.isNull){
          ret
        }
        else {
          cancelLeaf(oP.R,leaf,del)
        }
    }
  }
  
  private def trim_tree(leaf:UserUnits): UserUnits = leaf.P match {
    case Null() => Null()
    case Scalar() => Scalar()
    case Mul(_,_,l:UserUnits,r:UserUnits,p:UserUnits) => {
      val s:UserUnits = if(leaf==l) r else l
      val gp:UserUnits = leaf.P.P
      s.P = gp
      var ret:UserUnits = Null()
      gp match {
        case Null() => ret = s
        case Scalar() => ret = s
        case oP:Op => {
          if(oP.L == leaf.P){
            oP.L = s
          }
          else{
            oP.R = s
          }
          ret = get_root(gp) 
        }
      }
      ret
    }
    case Div(_,_,l:UserUnits,r:UserUnits,p:UserUnits) => {
      if(leaf==l) { leaf.P.asInstanceOf[Div].L = Null(); get_root(leaf.P)}  
      else {
      val gp:UserUnits = leaf.P.P
      l.P = gp
      var ret:UserUnits = Null()
      gp match {
        case Null() => ret = l
        case Scalar() => ret = l
        case oP:Op => {
          if(oP.L == leaf.P){
            oP.L = l
          }
          else{
            oP.R = l
          }
          ret = get_root(gp) 
        }
      }
      ret
      }
    }
  }
  
  /*Returns the root of a user units tree*/
  private def get_root(u:UserUnits):UserUnits = u.P match{
    case Null() => Null()
    case Scalar() => u
    case _ => get_root(u.P)
  }
  
  /*Corrects string and scale values after the user unit tree was been altered*/
  private def correctUU(uu:UserUnits):Unit = uu match{
    case oP:Op => {correctUU(oP.L); correctUU(oP.R);  if(!preserveAlias(oP)) {uu.scale = oP.L.scale*oP.R.scale; uu.str = oP.L.str*oP.R.str}}
    case _ => {}
  } 
  
  private def preserveAlias(uu:Op):Boolean = {
    if(!alias.contains(uu.str.toString)){
      false
    }
    else {
      val ualias = alias(uu.str.toString).uu 
      ualias match {
        case o:Op => if((o.L == uu.L) && (o.R == o.L)) {uu.scale = o.scale; true} else false     
        case _ => false
      }
    }
  }
  /*Copies a user unit. Used when retrieving an alias*/
  /*Necessary since user units are mutable*/
  private def copyUU(uu:UserUnits):UserUnits = uu match{
    case Null() => Null()
    case Scalar() => Scalar()
    case Leaf(str:UnitString,scale,_,fun) => Leaf(str,scale,Scalar(),fun)
    case Mul(str:UnitString,scale,l:UserUnits,r:UserUnits,_) => {val cl = copyUU(l); val cr = copyUU(r); val m = Mul(str,scale,cl,cr,Scalar()); cl.P = m; cr.P = m; m }
    case Div(str:UnitString,scale,l:UserUnits,r:UserUnits,_) => {val cl = copyUU(l); val cr = copyUU(r); val m = Div(str,scale,cl,cr,Scalar()); cl.P = m; cr.P = m; m }
  }
  
  /*parses an single unit into a fundamental representation + scale*/
  private def get_unit_scale(str:String):SUnitScale = {
    if(alias.contains(str)){
      SUnitScale(alias(str).uu.scale,alias(str).u)
    }
    else{
	  var scale = 1.0
	  var unit = SUnit(0,0,0,0,0,0,0)
	  val split = str.split("-")
	  if(split.length > 1){
	    scale = get_scale(split(0))
	    unit = get_unit(split(1))
	  }
	 else{
	    unit = get_unit(split(0))
	  }
	  SUnitScale(scale,unit)
    }
  }
  
  /*Scales for metric prefixes. nano to giga are supported*/
  private def get_scale(prefix:String):Double = prefix match {
    case "giga" => 1000000000
    case "mega" => 1000000
    case "kilo" => 1000
    case "hecto" => 100
    case "deka" => 10
    case "deci" =>  1/10
    case "centi" => 1/100
    case "milli" => 1/1000
    case "micro" => 1/1000000
    case "nano" =>  1/1000000000 
  }
  
  /*parses a single unit*/
  private def get_unit(str:String):SUnit = str match{
    case "grams" => SUnit(1, 0 , 0, 0, 0, 0, 0 )
    case "seconds" => SUnit(0, 1 , 0, 0, 0, 0, 0 )
    case "meters" => SUnit(0, 0 , 1, 0, 0, 0, 0 )
    case "moles" => SUnit(0, 0 , 0, 1, 0, 0, 0 )
    case "kelvin" => SUnit(0, 0 , 0, 0, 1, 0, 0 )
    case "ampheres" => SUnit(0, 0 , 0, 0, 0, 1, 0 )
    case "candelas" => SUnit(0, 0 , 0, 0, 0, 0, 1 )
    case _ => throw new IllegalUnitFormatException("ERROR: "+str+" is neither alias nor valid base unit")
  }
  
  /*implicit definitions*/
  implicit def symbol2Assignment(sym:Symbol) = Assignment(sym)
  implicit def int2Unit(i:Double) = unitBuilder(i)
  implicit def string2Alias(str:String) = Alias(str)
  implicit def string2AliasBuilder(str:String) = aliasUnitBuilder(str)
  implicit def sym2PrintBuilder(sym:Symbol) = printBuilder(sym)
  implicit def sym2Converter(sym:Symbol) = Converter(sym)
  implicit def sym2MathFunction(sym:Symbol) = MathFunction(() => svars(sym))
  implicit def sym2MathComparison(sym:Symbol) = Comparison(sym)
  implicit def fnOfSVal2MathFunction(fn:() => SVal) = MathFunction(fn)
}
