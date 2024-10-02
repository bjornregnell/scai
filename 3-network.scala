import mathematics.* 

/** A simple model of a brain with neurons in layers. */
class Network(val inputSize: Int, val layerSizes: List[Int]):
  val input   = new Vec(inputSize)
  val outputs = new Array[Vec](layerSizes.length)

  type Layer = Array[Neuron]
  val neurons = new Array[Layer](layerSizes.length)
  val lastLayer = layerSizes.length - 1
  
  for layer <- 0 until layerSizes.length do 
    // make room for neurons in layers and output vectors between layers
    neurons(layer) = new Layer(layerSizes(layer))
    outputs(layer) = new Vec(layerSizes(layer))
    for index <- 0 until layerSizes(layer) do 
      if layer == 0 
      then // neurons in the first layer are connected to input
        neurons(layer)(index) = new Neuron(input)
      else // other neurons are connected to the output of the previous layer
        neurons(layer)(index) = new Neuron(outputs(layer - 1))
  
  /** Walk through all neurons in all layers and forward outputs to next layer */
  def feedForward(): Unit = 
    for layer <- layerSizes.indices do
      for index <- outputs(layer).indices do
        outputs(layer)(index) = neurons(layer)(index).output()

  /** Use signal as input and feed forward to subsequent layers. */
  def predict(signal: Vec): Vec =
    for i <- input.indices do input(i) = signal(i)
    feedForward()
    outputs(lastLayer)
  
  /** Return layer and index of a randomly picked neuron in this network */
  def randomNeuron(): (Int, Int) = 
    val layer = util.Random.nextInt(layerSizes.length)
    val index = util.Random.nextInt(layerSizes(layer))
    (layer, index)
  
  /** Pick a random neuron and mutate its parameters.*/
  def mutateRandomNeuron(learningFactor: Num): (Int, Int) = 
    val (l, i) = randomNeuron()
    neurons(l)(i).mutate(learningFactor)
    (l, i)
  
  /** Run training steps using data. The learningFactor controls the size of mutations.*/
  def train(steps: Int, data: DataSet, learningFactor: Num = 0.3): Unit = 
    def computeError(): Num = 
      var averageError = 0.0
      var i = 0
      while i < data.size do
        val loss = meanSquaredError(predict(data.inputs(i)), data.correctOutputs(i))
        i += 1
        averageError = averageError + (loss - averageError)/i 
      end while
      averageError
    end computeError

    for step <- 1 to steps do
      val err1 = computeError()
      val (l, i) = mutateRandomNeuron(learningFactor)
      val err2 = computeError()
      if step % (steps / 10) == 0 then 
        println(f"step $step%3d; error before mutation: $err1%1.7f - after: $err2%1.7f")
      if err2 < err1
      then neurons(l)(i).save() 
      else neurons(l)(i).backtrack()
  end train
  
  /** Show this network with its neurons in each layer */
  def show: String = 
    val heading = s"Neural Network [$inputSize inputs, layer sizes: ${layerSizes.mkString(",")}]"
    var body = 
      (
        for layer <- 0 until layerSizes.length yield
          s"Layer $layer: " ++ (
            for index <- 0 until layerSizes(layer) 
            yield neurons(layer)(index).show
          ).mkString(", ")
      ).mkString("\n")
    s"$heading\n$body"
  
end Network