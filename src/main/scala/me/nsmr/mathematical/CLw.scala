package me.nsmr.mathematical

package object clw {
  trait Expression
  trait PrimitiveSymbol extends Expression
  sealed trait CLTerm extends Expression {
    def length: Int = 1
    def isClosed: Boolean = false
    def isCombinator: Boolean = false
    def fv: Set[Variable] = Set()
  }
  object CLTerm {
    def apply(obj: Any): CLTerm = {
      obj match {
        case (car, cdr) => ComplexTerm(CLTerm(car), CLTerm(cdr))
        case term: CLTerm => term
        case _ => throw new UnsupportedOperationException
      }
    }
  }
  trait Formula extends Expression {
    def left: CLTerm
    def right: CLTerm
    def operator: String
    override def toString() = s"$left $operator $right"
  }

  object Variable {
    var alloc_count = -1
    def alloc = {
      alloc_count = alloc_count + 1
      Variable("_G" + alloc_count)
    }
  }
  case class Variable(name: String) extends CLTerm with PrimitiveSymbol {
    override lazy val fv = Set(this)
    override def toString() = name
  }
  case class Constant(value: Any) extends CLTerm with PrimitiveSymbol {
    override val isClosed = true
    override lazy val isCombinator = value match {
      case "S" => true
      case "K" => true
      case _ => false
    }
    override def toString() = value.toString()
  }
  val S = Constant("S")
  val K = Constant("K")
  case object `(` extends PrimitiveSymbol
  case object `)` extends PrimitiveSymbol
  case class ComplexTerm(car: CLTerm, cdr: CLTerm) extends CLTerm {
    override lazy val length = car.length + cdr.length
    override lazy val isClosed = car.isClosed && cdr.isClosed
    override lazy val isCombinator = car.isCombinator && cdr.isCombinator
    override def fv = car.fv ++ cdr.fv
    override def toString(): String = {
      car.toString() + (cdr match {
        case cdr: ComplexTerm => s"($cdr)"
        case _ => cdr.toString()
      })
    }
  }
  case class `▷_1`(override val left: CLTerm, override val right: CLTerm) extends Formula {
    override def operator = "▷1"
  }
  val --> = `▷_1`
  case class ▷(override val left: CLTerm, override val right: CLTerm) extends Formula {
    override def operator = "▷"
  }
  case class `=`(override val left: CLTerm, override val right: CLTerm) extends Formula {
    override def operator = "="
  }

  trait Axiom
  object Axioms {
    val t = ComplexTerm
    case object Axiom_K extends Axiom {
      def apply(m: CLTerm, n: CLTerm): Formula = `▷_1`(ComplexTerm(ComplexTerm(K, m), n), m)
      def unapply(f: Formula): Option[(CLTerm, CLTerm)] = {
        f match {
          case (ComplexTerm(ComplexTerm(K, m1), n) `▷_1` m2) if m1 == m2 => Option((m1, n))
          case _ => None
        }
      }
    }
    case object Axiom_S extends Axiom {
      def apply(m: CLTerm, n: CLTerm, r: CLTerm): Formula = `▷_1`(t(t(t(S, m), n), r), t(t(m, r), t(n, r)))
      def unapply(f: Formula): Option[(CLTerm, CLTerm, CLTerm)] = {
        f match {
          case (t(t(t(S, m1), n1), r1) `▷_1` t(t(m2, r2), t(n2, r3))) if m1==m2 && n1==n2 && r1==r2 && r2==r3 =>
            Option((m1, n1, r1))
          case _ => None
        }
      }
    }
    case object Axiom_ρ extends Axiom {
      def apply(m: CLTerm): Formula = ▷(m, m)
      def unapply(f: Formula): Option[CLTerm] = {
        f match {
          case (m1 ▷ m2) if m1 == m2 => Option(m1)
          case _ => None
        }
      }
    }
  }

  trait Rule
  case object Rule_μ extends Rule {
    def apply(f: Formula, m: CLTerm): Formula = {
      f match {
        case (n --> r) => `▷_1`(ComplexTerm(m, n), ComplexTerm(m, r))
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[Formula] = {
      f match {
        case (ComplexTerm(m1, n) --> ComplexTerm(m2, r)) if m1==m2 =>
          Some(`▷_1`(n, r))
        case _ => None
      }
    }
  }
  case object Rule_ν extends Rule {
    def apply(f: Formula, r: CLTerm): Formula = {
      f match {
        case (m --> n) => -->(ComplexTerm(m, r), ComplexTerm(n, r))
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[Formula] = {
      f match {
        case (ComplexTerm(m, r1) --> ComplexTerm(n, r2)) if r1==r2 =>
          Some(-->(m, n))
        case _ => None
      }
    }
  }
  case object Rule_τ extends Rule {
    def apply(f1: Formula, f2: Formula): Formula = {
      (f1, f2) match {
        case ((m --> n1), (n2 ▷ r)) if n1==n2 => ▷(m, r)
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[(Formula, Formula)] = {
      f match {
        case (m ▷ r) =>
          val n = Variable.alloc
          Some((-->(m, n), ▷(n, r)))
        case _ => None
      }
    }
  }
  case object Rule_κ extends Rule {
    def apply(f: Formula): Formula = {
      f match {
        case (m ▷ n) => `=`(m, n)
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[Formula] = {
      f match {
        case (m `=` n) => Some(▷(m, n))
        case _ => None
      }
    }
  }
  case object Rule_σ extends Rule {
    def apply(f: Formula): Formula = {
      f match {
        case (m `=` n) => `=`(n, m)
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[Formula] = {
      f match {
        case (n `=` m) => Some(`=`(m, n))
        case _ => None
      }
    }
  }
  case object Rule_τeq extends Rule {
    def apply(f1: Formula, f2: Formula): Formula = {
      (f1, f2) match {
        case ((m `=` n1), (n2 `=` r)) if n1==n2 => `=`(m, r)
        case _ => throw new UnsupportedOperationException
      }
    }
    def unapply(f: Formula): Option[(Formula, Formula)] = {
      f match {
        case (m `=` r) =>
          val n = Variable.alloc
          Some((`=`(m, n), `=`(n, r)))
        case _ => None
      }
    }
  }
//  object Prover {
//    def apply(f: Formula): Boolean = {
//      import Axioms._
//      println(s"input: $f")
//      f match {
//        case Axiom_K(m) => true
//        case Axiom_S(m, n, r) => true
//        case Axiom_ρ(m) => true
//        case Rule_μ(f) => apply(f)
//        case Rule_ν(f) => apply(f)
//        case Rule_τ(f1, f2) => apply(f1) && apply(f2)
//        case Rule_κ(f) => apply(f)
//        case Rule_σ(f) => apply(f)
//        case Rule_τeq(f1, f2) => apply(f1) && apply(f2)
//        case _ => false
//      }
//    }
//  }
}