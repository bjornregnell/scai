package scai

class Network(nbrOfInputs: Int = 2, layerSizes: Seq[Int] = Seq(3,2,1)):

  val inputs = new Array[Float](nbrOfInputs)
  val outputs = new Array[Array[Float]](layerSizes.length)
  val neurons = new Array[Array[Neuron]](layerSizes.length)

  val nbrOfNeurons = layerSizes.sum
  val lastLayer = layerSizes.length - 1
  
  // allocate neurons in layers
  for layer <- 0 until layerSizes.length do 
    outputs(layer) = new Array[Float](layerSizes(layer))
    neurons(layer) = new Array[Neuron](layerSizes(layer))
    for index <- 0 until layerSizes(layer) do 
      if layer == 0 then 
        neurons(layer)(index) = new Neuron(layer, index, inputs, outputs(layer))
      else
        neurons(layer)(index) = new Neuron(layer, index, outputs(layer - 1), outputs(layer))
  
  /** Walk through all neurons in all layers and forward outputs to next layer*/
  def feedForward() = 
    for layer <- 0 until layerSizes.length do
      for index <- 0 until layerSizes(layer) do
        neurons(layer)(index).feedForward()

  def predict(input: Array[Float]): Array[Float] =
    assert(input.length == nbrOfInputs, s"in lengths must be $nbrOfInputs") 
    val it = input.iterator
    for i <- inputs.indices do inputs(i) = it.next
    feedForward()
    outputs(lastLayer)

  def show: String = 
    val heading = s"Network(nbrOfInputs=$nbrOfInputs, layerSizes=$layerSizes):"
    var body = 
      s"Input: ${inputs.mkString(",")}\n" ++ (
        for layer <- 0 until layerSizes.length yield
          s"Layer $layer: " ++ (
            for index <- 0 until layerSizes(layer) 
            yield neurons(layer)(index).show
          ).mkString(", ")
      ).mkString("\n")

    s"$heading\n$body\nOutput: ${outputs(lastLayer).mkString(",")}\n"

  def train(iterations: Int, input: Array[Array[Float]], correctOutput: Array[Array[Float]]): Unit = 
    var averageLoss = 0.0
    var i = 0
    while i < input.size do
      val loss = meanSquareLoss(predict(input(i)), correctOutput(i))
      i += 1
      averageLoss += (loss - averageLoss)/i // https://en.wikipedia.org/wiki/Moving_average
    
    ???
