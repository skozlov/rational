package com.github.skozlov.rational

case class Rational private(numerator: BigInt, denominator: BigInt) {
	def toPair: (BigInt, BigInt) = (numerator, denominator)
}

object Rational {
	def apply(numerator: BigInt, denominator: BigInt): Rational = {
		require(denominator != 0, "denominator cannot be zero")
		if (denominator < 0) {
			apply(-numerator, -denominator)
		} else {
			val gcd = numerator gcd denominator
			new Rational(numerator / gcd, denominator / gcd)
		}
	}

	implicit def fromBigInt[N](n: N)(implicit f: N => BigInt): Rational = Rational(n, 1)
}