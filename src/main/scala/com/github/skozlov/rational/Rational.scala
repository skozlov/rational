package com.github.skozlov.rational

case class Rational private(numerator: BigInt, denominator: BigInt) {
	def toPair: (BigInt, BigInt) = (numerator, denominator)

	def isWhole: Boolean = denominator == 1

	def asBigInt: Option[BigInt] = if (isWhole) Some(numerator) else None

	def integerAndFractionalParts: (BigInt, Rational) = {
		val divAndMod = numerator.bigInteger divideAndRemainder denominator.bigInteger
		(divAndMod(0), new Rational(divAndMod(1), denominator))
	}

	def integerPart: BigInt = integerAndFractionalParts._1

	def floor: BigInt = asBigInt match {
		case Some(bigInt) => bigInt
		case None =>
			val intPart = integerPart
			if (intPart >= 0) intPart else intPart - 1
	}

	def ceil: BigInt = asBigInt match {
		case Some(bigInt) => bigInt
		case None =>
			val intPart = integerPart
			if (intPart >= 0) intPart + 1 else intPart
	}
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

	implicit def fromBigDecimal[D](d: D)(implicit f: D => BigDecimal): Rational = d.toBigIntExact match {
		case Some(n) => fromBigInt(n)
		case None =>
			val numerator = (d.bigDecimal movePointRight d.scale).toBigInteger
			val denominator = BigInt(10) pow d.scale
			Rational(numerator, denominator)
	}
}