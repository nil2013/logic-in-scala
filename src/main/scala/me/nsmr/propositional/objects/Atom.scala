package me.nsmr.propositional.objects

package object implicitConversions {
  implicit def anyValToConstant[T<:AnyVal](obj: T) = Constant(obj)
  implicit def charSequenceToConstant[T<:CharSequence](obj: T) = Constant(obj)
}
object LogicUnit {
  private[objects] def unifyWithSet(me: LogicUnit): LogicUnit --> Result[LogicUnit] = {
    case Set(Complete, tail) =>
      me.unify(tail)
    case set @ Set(car, cdr) =>
      me.unify(car) match {
        case Success(car) => Success(set.copy(car = car))
        case Failure => Failure
      }
  }
}

abstract class LogicUnit {
  def unifyWithSet = LogicUnit.unifyWithSet(this)
  def unify: LogicUnit --> Result[LogicUnit]
}
case object Complete extends LogicUnit {
  override def toString = "true"
  def unify = { case _ => Failure }
}

case class Constant[T](value: T) extends LogicUnit {
  override def toString = value.toString
  def unify = unifyWithSet orElse {
    case c @ Constant(v) if v == this.value => Success(Complete)
    case _ => Failure
  }
  def :- (con: LogicUnit*) = {
    Imply(this, con.reduce((car, cdr) => Set(car, cdr)))
  }
}

case class Imply(head: LogicUnit, body: LogicUnit) extends LogicUnit {
  override def toString = head.toString() + " :- " + body.toString()
  def unify = unifyWithSet orElse {
    case h: LogicUnit =>
      head.unify(h) match {
        case Success(obj) =>
          println(s"$body implies $head")
          Success(Set(obj, this.body))
        case Failure => Failure
      }
    case _ => Failure
  }
}

case class Set(car: LogicUnit, cdr: LogicUnit) extends LogicUnit {
  override def toString = (car match {
    case c: Constant[_] => c.toString()
    case x              => s"(${x})"
  }) + ", " + cdr.toString()
  def unify = throw new UnsupportedOperationException
}