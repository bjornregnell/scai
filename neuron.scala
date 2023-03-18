class Neuron(val input: Vec):
  def show: String = s"Neuron[input.size=${input.size}]"

  var bias: Num = random()

  var weights: Vec = Array.fill(input.size)(random())

  /** Initiate this neuron to a random state. */
  def reset(): Unit =
    bias = random()
    var i = 0
    while i < weights.length do 
      weights(i) = random()
      i += 1
  
  /** Randomly adjust the state, scaled by factor. */
  def mutate(factor: Num): Unit = 
    bias +=  factor * random()
    var i = 0
    while i < input.size do 
      weights(i) += factor * random()
      i += 1

  /** Compute output value. The sigmoid call constrains output in [0..1] */ 
  def output(): Num = sigmoid(multiply(weights, input) + bias)

  /** Memory to save the current bias*/
  var savedBias: Num = bias

  /** Memory to save the current weights*/
  var savedWeights: Vec = weights.clone()
  
  /** Forget current state and restore saved state */
  def backtrack(): Unit = 
    bias = savedBias
    var i = 0
    while i < input.size do 
      weights(i) = savedWeights(i)
      i += 1
  
  /** Remember current state. */
  def save(): Unit =
    savedBias = bias
    var i = 0
    while i < input.size do 
      savedWeights(i) = weights(i)
      i += 1
