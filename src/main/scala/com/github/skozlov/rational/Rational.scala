package com.github.skozlov.rational

import Rational._
import com.github.skozlov.bigint.leastCommonMultipleOf

import scala.annotation.tailrec

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

	def toPositional(radix: Byte): String = {
		require(radix >= 2 && radix <= 36, "radix should be between 2 and 36")
		if (numerator < 0) {
			'-' + (-this).toPositional(radix)
		}
		else if (isWhole) {
			numerator.toString(radix)
		}
		else {
			val (intPart, fracPart) = integerAndFractionalParts

			@tailrec
			def fracPartToString(rest: BigInt, pastRestsReversed: List[BigInt], restToDigit: Map[BigInt, Byte]): String = {
				def restsToString(rests: List[BigInt]): String = {
					def restToChar(rest: BigInt): Char = {
						val digit = restToDigit(rest)
						(if (digit < 10) '0' + digit else 'a' + (digit - 10)).toChar
					}

					(rests map restToChar).mkString
				}

				if (rest == 0) {
					restsToString(pastRestsReversed.reverse)
				}
				else if (restToDigit contains rest) {
					val rests = pastRestsReversed.reverse
					val (nonPeriodicRests, periodicRests) = rests splitAt (rests indexOf rest)
					s"${restsToString(nonPeriodicRests)}(${restsToString(periodicRests)})"
				}
				else {
					val divAndMod = (rest * BigInt(radix)).bigInteger divideAndRemainder this.denominator.bigInteger
					val digit = divAndMod(0).byteValueExact()
					val newRest = divAndMod(1)
					fracPartToString(newRest, rest :: pastRestsReversed, restToDigit + (rest -> digit))
				}
			}

			s"$intPart.${fracPartToString(fracPart.numerator, Nil, Map())}"
		}
	}

	def unary_- : Rational = new Rational(-numerator, denominator)

	def abs: Rational = if (numerator >= 0) this else -this

	def +(that: Rational): Rational = { // todo test
		val ((numerator1, denominator), (numerator2, _)) = toCommonDenominator(this, that)
		new Rational(numerator1 + numerator2, denominator)
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

	def fromPositional(source: String, radix: Byte): Rational = {
		require(radix >= 2 && radix <= 36, "radix should be between 2 and 36")

		def parseNonNegative(source: String): Rational = {
			def throwNumberFormatException = throw new NumberFormatException("Illegal number format: " + source)

			def parseNotNegativeInt(source: String): BigInt = {
				val result = BigInt(source, radix)
				if (result < 0) throwNumberFormatException else result
			}

			val intAndFrac = source.split("\\.", 2)
			val intPart = parseNotNegativeInt(intAndFrac(0))
			if (intAndFrac.size == 1) {
				intPart
			}
			else {
				val fracParts = intAndFrac(1).split("\\(", 2)
				val nonPeriodicPartSource = fracParts(0)
				val nonPeriodicPart = {
					if (nonPeriodicPartSource.isEmpty) fromBigInt(0)
					else Rational(parseNotNegativeInt(nonPeriodicPartSource), BigInt(radix) pow nonPeriodicPartSource.length)
				}
				if (fracParts.size == 1) {
					intPart + nonPeriodicPart
				}
				else {
					if (!(fracParts(1) endsWith ")")) {
						throwNumberFormatException
					}
					val periodicPartSource = fracParts(1) dropRight 1
					val periodicPart = Rational(
						parseNotNegativeInt(periodicPartSource),
						(BigInt(radix) pow nonPeriodicPartSource.length) * ((BigInt(radix) pow periodicPartSource.length) - 1)
					)
					intPart + nonPeriodicPart + periodicPart
				}
			}
		}

		if (source startsWith "-") -parseNonNegative(source drop 1) else parseNonNegative(source)
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