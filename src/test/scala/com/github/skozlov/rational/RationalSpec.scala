package com.github.skozlov.rational

import Rational.toCommonDenominator
import com.github.skozlov.Spec

class RationalSpec extends Spec {
	"Rational(BigInt, BigInt)" should "leave canonical fractions as is" in {
		Rational(2, 3).toPair shouldBe (2, 3)
		Rational(3, 2).toPair shouldBe (3, 2)
		Rational(2, 1).toPair shouldBe (2, 1)
	}

	"Rational(BigInt, BigInt)" should "reduce fractions" in {
		Rational(12, 8).toPair shouldBe (3, 2)
		Rational(8, 12).toPair shouldBe (2, 3)
		Rational(4, 2).toPair shouldBe (2, 1)
		Rational(2, 4).toPair shouldBe (1, 2)
	}

	"Rational(BigInt, BigInt)" should "always create fractions with positive denominators" in {
		Rational(-8, 12).toPair shouldBe (-2, 3)
		Rational(8, -12).toPair shouldBe (-2, 3)
		Rational(-8, -12).toPair shouldBe (2, 3)
	}

	"Rational(BigInt, BigInt)" should "create equal zeroes for any input denominator" in {
		Rational(0, 1).toPair shouldBe (0, 1)
		Rational(0, 2).toPair shouldBe (0, 1)
		Rational(0, -2).toPair shouldBe (0, 1)
	}

	"Rational(BigInt, BigInt)" should "throw IllegalArgumentException for zero denominator" in {
		for (numerator <- List(0, 1)) {
			intercept[IllegalArgumentException]{Rational(numerator, 0)}.getMessage shouldBe "requirement failed: denominator cannot be zero"
		}
	}

	"Rational" should "be implicitly convertible from BigInt or something implicitly convertible to BigInt" in {
		import Rational.fromBigInt
		(BigInt(Long.MaxValue) + 1).toPair shouldBe (BigInt(Long.MaxValue) + 1, 1)
		0.toPair shouldBe (0, 1)
		2.toPair shouldBe (2, 1)
		(-2).toPair shouldBe (-2, 1)
	}

	"Rational" should "be implicitly convertible from BigDecimal or something implicitly convertible to BigDecimal" in {
		import Rational.fromBigDecimal
		0.toPair shouldBe (0, 1)
		10.toPair shouldBe (10, 1)
		(-10).toPair shouldBe (-10, 1)
		0.5.toPair shouldBe (1, 2)
		(-0.5).toPair shouldBe (-1, 2)
	}

	"asBigInt" should "be Some(numerator) for a whole number" in {
		import Rational.fromBigInt
		for (n <- List(0, 10, -10)) {
			n.asBigInt shouldBe Some(n)
		}
	}

	"asBigInt" should "be None for a non-whole number" in {
		import Rational.fromBigDecimal
		for (r <- List(0.5, -0.5, 1.5, -1.5)) {
			r.asBigInt shouldBe None
		}
	}

	"isWhole" should "be true for a whole number" in {
		import Rational.fromBigInt
		for (n <- List(0, 10, -10)) {
			val r: Rational = n
			r.isWhole shouldBe true
		}
	}

	"isWhole" should "be false for a non-whole number" in {
		import Rational.fromBigDecimal
		for (d <- List(0.5, -0.5, 1.5, -1.5)) {
			val r: Rational = d
			r.isWhole shouldBe false
		}
	}

	"integerAndFractionalParts" should "return the integer part (the number rounded towards zero) and the fractional part as Rational" in {
		import Rational.fromBigDecimal
		0.integerAndFractionalParts shouldBe (0, fromBigDecimal(0))
		0.5.integerAndFractionalParts shouldBe (0, fromBigDecimal(0.5))
		(-0.5).integerAndFractionalParts shouldBe (0, fromBigDecimal(-0.5))
		2.integerAndFractionalParts shouldBe (2, fromBigDecimal(0))
		(-2).integerAndFractionalParts shouldBe (-2, fromBigDecimal(0))
		2.5.integerAndFractionalParts shouldBe (2, fromBigDecimal(0.5))
		(-2.5).integerAndFractionalParts shouldBe (-2, fromBigDecimal(-0.5))
	}

	"floor" should "return the largest BigInt that is less or equal to this Rational" in {
		import Rational.fromBigDecimal
		fromBigDecimal(0).floor shouldBe 0
		fromBigDecimal(2).floor shouldBe 2
		fromBigDecimal(-2).floor shouldBe -2
		fromBigDecimal(2.1).floor shouldBe 2
		fromBigDecimal(-2.1).floor shouldBe -3
		fromBigDecimal(2.5).floor shouldBe 2
		fromBigDecimal(-2.5).floor shouldBe -3
		fromBigDecimal(2.9).floor shouldBe 2
		fromBigDecimal(-2.9).floor shouldBe -3
	}

	"ceil" should "return the least BigInt that is greater that or equal to this Rational" in {
		import Rational.fromBigDecimal
		fromBigDecimal(0).ceil shouldBe 0
		fromBigDecimal(2).ceil shouldBe 2
		fromBigDecimal(-2).ceil shouldBe -2
		fromBigDecimal(2.1).ceil shouldBe 3
		fromBigDecimal(-2.1).ceil shouldBe -2
		fromBigDecimal(2.5).ceil shouldBe 3
		fromBigDecimal(-2.5).ceil shouldBe -2
		fromBigDecimal(2.9).ceil shouldBe 3
		fromBigDecimal(-2.9).ceil shouldBe -2
	}

	"round" should "return the nearest BigInt" in {
		import Rational.fromBigDecimal
		fromBigDecimal(0).round shouldBe 0
		fromBigDecimal(2).round shouldBe 2
		fromBigDecimal(-2).round shouldBe -2
		fromBigDecimal(2.1).round shouldBe 2
		fromBigDecimal(-2.1).round shouldBe -2
		fromBigDecimal(2.5).round shouldBe 3
		fromBigDecimal(-2.5).round shouldBe -3
		fromBigDecimal(2.9).round shouldBe 3
		fromBigDecimal(-2.9).round shouldBe -3
	}

	"ordering" should "work as expected" in {
		import Rational.fromBigDecimal
		val sortedCorrectly = List[Rational](Rational(-1, 2), Rational(-1, 3), 0, Rational(1, 3), Rational(1, 2))
		sortedCorrectly.sorted shouldBe sortedCorrectly
		for (r <- sortedCorrectly) {
			r compare r shouldBe 0
		}
		for (i <- sortedCorrectly.indices.tail) {
			sortedCorrectly(i - 1) should be < sortedCorrectly(i)
		}
	}

	"Rational.toCommonDenominator" should "convert two fractions to a least non-negative common denominator" in {
		toCommonDenominator(Rational(1, 2), Rational(3, 2)) shouldBe ((1, 2), (3, 2))
		toCommonDenominator(Rational(-1, 2), Rational(3, 2)) shouldBe ((-1, 2), (3, 2))
		toCommonDenominator(Rational(-1, 2), Rational(-3, 2)) shouldBe ((-1, 2), (-3, 2))
		toCommonDenominator(Rational(3, 4), Rational(5, 6)) shouldBe ((9, 12), (10, 12))
		toCommonDenominator(Rational(-3, 4), Rational(5, 6)) shouldBe ((-9, 12), (10, 12))
		toCommonDenominator(Rational(-3, 4), Rational(-5, 6)) shouldBe ((-9, 12), (-10, 12))
	}
}