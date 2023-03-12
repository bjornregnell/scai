package scai

val RNG = java.util.Random()

/** S-shape sigmoid function https://en.wikipedia.org/wiki/Sigmoid_function **/
inline def sigmoid(x: Double): Double = 1 / (1 + math.exp(-x))

//https://towardsdatascience.com/derivative-of-the-sigmoid-function-536880cf918e

inline def sigmoidDerived(x: Double): Double = 
  val sx = sigmoid(x) 
  sx * (1 - sx)

/** A random decimal number between -1.0 and 1.0 **/
inline def rnd(): Double = RNG.nextDouble() * 2 - 1

/** A random decimal with normal distribution, mean 0, standard deviation 1 **/ 
inline def gauss(): Double = RNG.nextGaussian()

def time[A](code: => A): (A, Long) = 
  val t0 = System.nanoTime()
  (code, System.nanoTime() - t0)

def dotProduct(xs: Array[Double], ys: Array[Double]): Double =
  var result = 0.0
  var i = 0
  while i < xs.length do
    result = result + xs(i) * ys(i)
    i += 1
  result

extension (xs: Array[Double]) inline def *(ys: Array[Double]) = dotProduct(xs, ys)

def meanSquareLoss(correct: Array[Double], predicted: Array[Double]): Double =
  var sumOfSquares = 0.0
  var i = 0
  while i < correct.length do
    val error = correct(i) - predicted(i)
    sumOfSquares += error * error
    i += 1
  sumOfSquares / correct.length
