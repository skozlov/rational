package com.github.skozlov.rational

import Rational._
import com.github.skozlov.bigint.leastCommonMultipleOf

case class Rational private(numerator: BigInt, denominator: BigInt) extends Ordered[Rational]{
	def asRational: Rational = this // to force an implicit conversion

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

	def round: BigInt = {
		val (intPart, fracPart) = integerAndFractionalParts
		if (intPart >= 0) {
			if (fracPart < new Rational(1, 2)) intPart else intPart + 1
		} else {
			if (fracPart > new Rational(-1, 2)) intPart else intPart - 1
		}
	}

	override def compare(that: Rational): Int = {
		toCommonDenominator(this, that) match {
			case ((numerator1, _), (numerator2, _)) => numerator1 compare numerator2
		}
	}

	override def toString: String = if (isWhole) numerator.toString else s"$numerator/$denominator"

	def unary_- : Rational = new Rational(-numerator, denominator)

	def abs: Rational = if (numerator >= 0) this else -this
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

	def toCommonDenominator(a: Rational, b: Rational): ((BigInt, BigInt), (BigInt, BigInt)) = {
		val denominator = leastCommonMultipleOf(a.denominator, b.denominator)
		val numerator1 = a.numerator * denominator / a.denominator
		val numerator2 = b.numerator * denominator / b.denominator
		((numerator1, denominator), (numerator2, denominator))
	}
}