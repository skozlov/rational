package com.github.skozlov.rational

class RationalSpec extends Spec {
	"Rational(BigInt, BigInt)" should "leave canonical fractions as is" in {
		Rational(2, 3).toPair shouldBe (2, 3)
		Rational(3, 2).toPair shouldBe (3, 2)
		Rational(2, 1).toPair shouldBe (2, 1)
	}

	it should "reduce fractions" in {
		Rational(12, 8).toPair shouldBe (3, 2)
		Rational(8, 12).toPair shouldBe (2, 3)
		Rational(4, 2).toPair shouldBe (2, 1)
		Rational(2, 4).toPair shouldBe (1, 2)
	}

	it should "always create fractions with positive denominators" in {
		Rational(-8, 12).toPair shouldBe (-2, 3)
		Rational(8, -12).toPair shouldBe (-2, 3)
		Rational(-8, -12).toPair shouldBe (2, 3)
	}

	it should "create equal zeroes for any input denominator" in {
		Rational(0, 1).toPair shouldBe (0, 1)
		Rational(0, 2).toPair shouldBe (0, 1)
		Rational(0, -2).toPair shouldBe (0, 1)
	}

	it should "throw IllegalArgumentException for zero denominator" in {
		for (numerator <- List(0, 1)) {
			intercept[IllegalArgumentException]{Rational(numerator, 0)}.getMessage shouldBe "requirement failed: denominator cannot be zero"
		}
	}
}