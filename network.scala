package scai

class Network(layerSizes: Seq[Int] = Seq(3,2,1), nbrOfInputs: Int = 2):

  val inputs = new Array[Float](nbrOfInputs)

  val layers: Array[Array[Neuron]] = 
    val xss = new Array[Array[Neuron]](layerSizes.length)
    xss(0) = Array.fill(layerSizes(0))(InputNeuron(inputs))
    for i <- 1 until layerSizes.length do
      xss(i) = Array.fill(layerSizes(i)):
        ConnectedNeuron(previousLayer = xss(i - 1))
    xss

  val outputs: Array[Float] = new Array[Float](layerSizes.last)

  def predict(): Unit =
    var i = 0
    while i < outputs.size do
      for neuron <- layers.last do 
        neuron.feed()
        outputs(i) = neuron.output()
      i += 1
