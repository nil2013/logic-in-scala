package me.nsmr.propositional.objects

object Result {
  implicit def resultToBoolean(res: Result[LogicUnit]) = res.isSuccess
}
abstract class Result[+T<:LogicUnit] {
  def isSuccess: Boolean = false
  final def isFailure = !isSuccess
  def get: T = throw new NoSuchElementException
}

case class Success[+T<:LogicUnit](obj: T) extends Result[T] {
  override def isSuccess = true
  override def get = obj
}
case object Failure extends Result[Nothing]