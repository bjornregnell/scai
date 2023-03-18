/** A decimal number with double precision */
type Num = Double

/** A vector with many numbers */
type Vec = Array[Num]

def multiply(x: Vec, y: Vec): Num =
  var result = 0.0
  for i <- x.indices do 
    result = result + x(i) * y(i)
  end for
  result

def meanSquareError(correct: Vec, predicted: Vec): Num =
  var sumOfSquares = 0.0
  for i <- correct.indices do
    val error = correct(i) - predicted(i)
    sumOfSquares = sumOfSquares + error * error
  end for
  sumOfSquares / correct.size

/** An S-shaped function that scales the input to a numnber between 0.0 and 1.0
  * https://en.wikipedia.org/wiki/Sigmoid_function **/
inline def sigmoid(x: Num): Num = (1 / (1 + math.exp(-x)))

/** A Random Number Generator*/
val RNG = new java.util.Random()

/** A random number with normal distribution, mean 0, standard deviation 1 **/ 
def random(): Num = RNG.nextGaussian()