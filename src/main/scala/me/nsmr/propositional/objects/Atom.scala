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
  private[objects] def unifyWithOr(me: LogicUnit): LogicUnit --> Result[LogicUnit] = {
    case Or(head, tail) =>
      me.unify(head) match {
        case Failure => me.unify(tail)
        case result  => result
      }
  }
}
abstract class LogicUnit {
  def unifyWithSet = LogicUnit.unifyWithSet(this)
  def unifyWithOr = LogicUnit.unifyWithOr(this)
  def unify: LogicUnit --> Result[LogicUnit]
}

case object Complete extends LogicUnit {
  override def toString = "true"
  def unify = { case _ => Failure }
}

case class Constant[T](value: T) extends LogicUnit {
  override def toString = value.toString
  def unify = unifyWithSet orElse unifyWithOr orElse {
    case c @ Constant(v) if v == this.value => Success(Complete)
    case _ => Failure
  }
  def :- (con: LogicUnit*) = {
    Imply(this, con.reduce((car, cdr) => Set(car, cdr)))
  }
  def or(tail: LogicUnit) = Or(this, tail)
}

case class Imply(head: LogicUnit, body: LogicUnit) extends LogicUnit {
  override def toString = head.toString() + " :- " + body.toString()
  def unify = unifyWithSet orElse unifyWithOr orElse {
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
  def or(tail: LogicUnit) = Or(this, tail)
}

case class Or(head: LogicUnit, tail: LogicUnit) extends LogicUnit {
  override def toString = (head match {
    case c: Constant[_] => c.toString()
    case x              => s"(${x})"
  }) + " or " + tail.toString()
  def unify = {
    case Set(car, cdr) =>
      head.unify(car) match {
        case Failure => tail.unify(car)
        case result  => result
      }
    case h: LogicUnit =>
      head.unify(h) match {
        case Failure => tail.unify(h)
        case result  => result
      }
    case _ => Failure
  }
}