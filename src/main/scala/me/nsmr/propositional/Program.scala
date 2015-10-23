package me.nsmr.propositional

import objects._
import scala.annotation.tailrec
import implicitConversions._

object Program {
  def getTestProgram = {
    Program (
        "test3"
        , "test4"
        , "test" :- "test2"
        , "test" :- ("test3", "test4")
        , "test5" :- ("test4")
        , "test6" :- ("test3", "test")
        , "test7" :- ("test6", "test5")
        )
  }
}

case class Program(lines: LogicUnit*) {
  def source = lines.map(_.toString() + ".").mkString("\n")
  
  def find(question: LogicUnit) = {
    new Iterator[Result[LogicUnit]] {
      val it = lines.iterator
      def read: Result[LogicUnit] = {
        if(!it.hasNext) Failure
        else {
          it.next().unify(question) match {
            case sc : Success[_] => sc
            case _ => read
          }
        }
      }
      private[this] var last: Result[LogicUnit]  = null
      def hasNext = (last == null || last.isSuccess)
      def next = {
        if(hasNext) last = read
        last
      }
    }
  }

  def ?- (question: LogicUnit): Iterator[Result[LogicUnit]] = {
    println(s"$question?")
    question match {
      case Complete =>
        SingleIterator(Success(Complete))
      case Set(Complete, cdr) => Program.this ?- cdr
      case question =>
        new Iterator[Result[LogicUnit]] {
          val iterator = lines.iterator
          private[this] var lineIterator: Iterator[Result[LogicUnit]] = null
          private[this] var last: Result[LogicUnit] = null
          def hasNext = {
            // 前回がSuccessだったらhasNext
            if(last == null) true else last
          }
          def read: Result[LogicUnit] = {
            if(lineIterator != null && lineIterator.hasNext) {
              lineIterator.next match {
                case sc: Success[_] => sc
                case _ => read
              }
            } else {
              if(!iterator.hasNext) {
                println("failure...")
                Failure
              }
              else {
                iterator.next.unify(question) match {
                  case Success(subgoal) => lineIterator = (Program.this ?- subgoal)
                  case _ => lineIterator = null
                }
                read
              }
            }
          }
          def next: Result[LogicUnit] = {
            last = read
            last
          }
        }
    }
  }
}