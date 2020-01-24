// Start writing your ScalaFiddle code here

def präfix (ys:List[Any], xs:List[Any]) : Boolean = ys match {
  case Nil => true
  case z::zs => xs match {
    case Nil => false
    case z1::z1s => if(z1!=z) false else präfix(zs, z1s)
  }
}

def infix(xs:List[Any], ys:List[Any]) : Boolean = ys match {
  case Nil => (xs==Nil)
  case z::zs => präfix(xs,ys) || infix(xs,zs)
}

val result = infix(1::3::Nil, 2::1::3::4::Nil)



//TESTS in der KLausur möglichst immer alle Fälle: leere Liste, keine leere Liste, Infix vorhanden, keines Vorhanden
