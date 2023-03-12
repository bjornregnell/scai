package scai

def InputNeuron(inputs: Array[Float]): Neuron = Neuron(inputs: Array[Float], Array())

def ConnectedNeuron(previousLayer: Array[Neuron]): Neuron =
  new Neuron(new Array[Float](previousLayer.size), previousLayer)

class Neuron(val inputs: Array[Float], val previousLayer: Array[Neuron]):
  def size = inputs.size

  assert(previousLayer.size == 0 || previousLayer.size == size,
    "size of inputs must match numer of neurons in previous layer")

  var bias: Float = gauss()
  
  var weights: Array[Float] = Array.fill(size)(gauss())

  def reset(): Unit =
    bias = gauss()
    var i = 0
    while i < weights.length do 
      weights(i) = gauss()
      i += 1
  
  def adjust(deltaBias: Float, deltaWeights: Array[Float]): Unit =
    bias -= deltaBias
    var i = 0
    while i < size do
      weights(i) -= deltaWeights(i)
      i += 1

  def mutate(factor: Float = 1.0): Unit = 
    bias +=  factor * gauss()
    var i = 0
    while i < size do 
      weights(i) += factor * gauss()
      i += 1
  
  def output(): Float = sigmoid(weights * inputs + bias)
  def outputDerived(): Float = sigmoidDerived(weights * inputs + bias)

  def feed(): Unit = 
    var i = 0
    while i < previousLayer.size do
      previousLayer(i).feed()
      inputs(i) = previousLayer(i).output()
      i += 1


trait Memory:
  self : Neuron =>

  var oldBias: Float = bias
  var oldWeights: Array[Float] = weights.clone()

  def forget(): Unit =
    bias = oldBias
    var i = 0
    while i < size do 
      weights(i) = oldWeights(i)
      i += 1

  def remember(): Unit =
    oldBias = bias
    var i = 0
    while i < size do 
      oldWeights(i) = weights(i)
      i += 1
