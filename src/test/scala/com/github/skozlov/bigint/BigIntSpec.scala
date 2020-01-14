package com.github.skozlov.bigint

import com.github.skozlov.Spec

class BigIntSpec extends Spec {
	"leastCommonMultipleOf" should "throw IllegalArgumentException for a zero argument" in {
		for ((a, b) <- List((0, 0), (0, 1), (1, 0))) {
			intercept[IllegalArgumentException]{leastCommonMultipleOf(a, b)}.getMessage shouldBe "requirement failed: zero argument"
		}
	}

	"leastCommonMultipleOf" should "return the least non-negative common multiple of two numbers" in {
		leastCommonMultipleOf(4, 4) shouldBe 4
		leastCommonMultipleOf(-4, 4) shouldBe 4
		leastCommonMultipleOf(4, -4) shouldBe 4
		leastCommonMultipleOf(-4, -4) shouldBe 4
		leastCommonMultipleOf(4, 2) shouldBe 4
		leastCommonMultipleOf(4, 3) shouldBe 12
		leastCommonMultipleOf(4, 6) shouldBe 12
	}
}