package me.nsmr.mathematical

import org.scalatest.{ GivenWhenThen, FeatureSpec }

class PrologSampleTest extends FeatureSpec with GivenWhenThen {
  feature("""CLw Logic Test""") {
    import clw._
    val S = Constant("S")
    val x = Variable("x")
    val y = Variable("y")
    val μ = (0 to 10).map(i => Variable(s"μ_$i"))

    scenario("FV(SxyS) must be {x, y}.") {
      val SxyS = CLTerm((((S, x),y),S))
      assert(SxyS.fv == Set(x,y))
    }
    scenario("Formula 'S(μ_0μ_1)(μ_0SK)μ_1 ▷_1 μ_0μ_1μ_1(μ_0SKμ_1)' must be the instance of Axiom (K).") {
      val instance = `▷_1`(CLTerm(
          (((S, (μ(0), μ(1))), ((μ(0), S), K)), μ(1))
          ), CLTerm(
          (((μ(0), μ(1)), μ(1)), (((μ(0), S), K), μ(1)))
          ))
      println(instance)
      assert(instance match {
        case Axioms.Axiom_S(m) => true
        case _ => false
      })
    }
//    scenario("Formula 'S(μ_0μ_1)(μ_0SK)μ_1 ▷_1 μ_0μ_1μ_1(μ_0SKμ_1)' must be provable.") {
//      val proposition = `▷_1`(CLTerm(
//          (((S, (μ(0), μ(1))), ((μ(0), S), K)), μ(1))
//          ), CLTerm(
//          (((μ(0), μ(1)), μ(1)), (((μ(0), S), K), μ(1)))
//          ))
////      println(proposition)
//      assert(Prover(proposition))
//    }
    scenario("Formula 'S(Kμ_0)μ_1μ_2▷μ_0(μ_1μ_2)' must be provable by s, k, ρ.") {
      val s = Axioms.Axiom_S(CLTerm((K, μ(0))), μ(1), μ(2))
      val k = Axioms.Axiom_K(μ(0), μ(2))
      val ρ = Axioms.Axiom_ρ(CLTerm((μ(0), (μ(1), μ(2)))))
      val proposition = ▷(CLTerm(
          (((S, (K, μ(0))), μ(1)), μ(2) )
          ), CLTerm(
          ( μ(0), (μ(1), μ(2)) )
          ))
      println(s"s: $s, k: $k, ρ: $ρ, P: $proposition")
      val result = Rule_τ(s, Rule_τ(Rule_ν(k, CLTerm((μ(1), μ(2)))), ρ))
      println(s"result: $result")
      assert(result == proposition)
    }
//    scenario("Formula 'S(Kμ_0)μ_1μ_2▷μ_0(μ_1μ_2)' must be provable.") {
//      val proposition = ▷(CLTerm(
//          (((S, (K, μ(0))), μ(1)), μ(2) )
//          ), CLTerm(
//          ( μ(0), (μ(1), μ(2)) )
//          ))
//      println(proposition)
//      assert(Prover(proposition))
//    }
  }
}