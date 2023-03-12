package scai

val RNG = java.util.Random()

/** Faster exp based on Taylor series. Error less than 0.001%  within [-1..+1] 
  * https://stackoverflow.com/a/10552567/10088022
  **/
def exp(x: Float): Float = 
  (5040+x*(5040+x*(2520+x*(840+x*(210+x*(42+x*(7+x)))))))*0.00019841269F

/** S-shape sigmoid function https://en.wikipedia.org/wiki/Sigmoid_function **/
inline def sigmoid(x: Float): Float = (1 / (1 + math.exp(-x.toDouble))).toFloat

//https://towardsdatascience.com/derivative-of-the-sigmoid-function-536880cf918e

inline def sigmoidDerived(x: Float): Float = 
  val sx = sigmoid(x) 
  sx * (1 - sx)

/** A random decimal number between -1.0 and 1.0 **/
inline def rnd(): Float = RNG.nextFloat() * 2 - 1

/** A random decimal with normal distribution, mean 0, standard deviation 1 **/ 
inline def gauss(): Float = RNG.nextGaussian().toFloat

def dotProduct(xs: Array[Float], ys: Array[Float]): Float =
  var result: Float = 0.0F
  var i = 0
  while i < xs.length do
    result = result + xs(i) * ys(i)
    i += 1
  result

extension (xs: Array[Float]) inline def *(ys: Array[Float]) = dotProduct(xs, ys)

def meanSquareLoss(correct: Array[Float], predicted: Array[Float]): Float =
  var sumOfSquares: Float = 0.0F
  var i = 0
  while i < correct.length do
    val error = correct(i) - predicted(i)
    sumOfSquares += error * error
    i += 1
  sumOfSquares / correct.length


def time[A](code: => A): (A, Long) = 
  val t0 = System.nanoTime()
  (code, System.nanoTime() - t0)

def timeAvg[A](n: Int = 1_000_000, warmup: Int = 1000)(code: => A): Double = 
  var i = 0
  var avg = 0.0
  while i < warmup do
    code
    i += 1
  i = 0
  while i < warmup do
    val t0 = System.nanoTime()
    code
    val t = System.nanoTime() - t0
    i += 1
    avg += (t - avg)/i // https://en.wikipedia.org/wiki/Moving_average
  avg