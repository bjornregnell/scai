package neuronisc

class Neuron(size: Int):
  var bias: Double = 0.1
  var weights: Array[Double] = Array.fill(size)(0.1)