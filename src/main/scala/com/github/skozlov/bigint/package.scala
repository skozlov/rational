package com.github.skozlov

package object bigint {
	def leastCommonMultipleOf(a: BigInt, b: BigInt): BigInt = {
		require(a != 0, "zero argument")
		require(b != 0, "zero argument")
		(a * b).abs / (a gcd b)
	}
}