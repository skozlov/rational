package com.github.skozlov.rational

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
}