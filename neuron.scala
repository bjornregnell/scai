package scai

class Neuron(size: Int):
  var bias: Double = gauss()
  
  var weights: Array[Double] = Array.fill(size)(gauss())

  def reset(): Unit =
    bias = gauss()
    var i = 0
    while i < weights.length do 
      weights(i) = gauss()
      i += 1

  def output(input: Array[Double]): Double = sigmoid(weights * input + bias)
  
  def outputDerived(input: Array[Double]): Double = sigmoidDerived(weights * input + bias)
  
  def show: String = s"$toString[bias=$bias, size=$size]"
  
  def adjust(deltaBias: Double, deltaWeights: Array[Double]): Unit =
    bias -= deltaBias
    var i = 0
    while i < weights.length do
      weights(i) -= deltaWeights(i)
      i += 1