import mathematics.* 

/** A simple model of a brain cell. */
class Neuron(val input: Vec):
  var bias: Num = random()
  var weights: Vec = Array.fill(input.size)(random())

  /** Randomly adjust the state, scaled by factor. */
  def mutate(factor: Num): Unit = 
    bias = bias + factor * random()
    for i <- weights.indices do 
      weights(i) = weights(i) + factor * random()

  /** Compute output value. The sigmoid constrains output within [0..1]. */ 
  def output(): Num = 
    val x = multiply(weights, input) + bias 
    sigmoid(x)

  /** Memory for saving the current bias. */
  var savedBias: Num = bias

  /** Memory for saving the current weights. */
  var savedWeights: Vec = weights.clone()
  
  /** Forget current state and restore saved state. */
  def backtrack(): Unit = 
    bias = savedBias
    for i <- weights.indices do 
      weights(i) = savedWeights(i)
  
  /** Remember current state. */
  def save(): Unit =
    savedBias = bias
    for i <- weights.indices do 
      savedWeights(i) = weights(i)
  
  def show: String = s"Neuron[input.size=${input.size}]"
end Neuron