type Layer = Array[Neuron]

/** A simple model of a brain with several neurons in several layers. */
class Network(val inputSize: Int, val layerSizes: List[Int]):
  val input = new Vec(inputSize)
  val outputs = new Array[Vec](layerSizes.length)
  val neurons = new Array[Layer](layerSizes.length)

  def randomNeuron(): (Int, Int) = 
    val layer = util.Random.nextInt(layerSizes.length)
    val index = util.Random.nextInt(layerSizes(layer))
    (layer, index)

  val lastLayer = layerSizes.length - 1
  
  // allocate neurons in layers
  for layer <- 0 until layerSizes.length do 
    outputs(layer) = new Vec(layerSizes(layer))
    neurons(layer) = new Layer(layerSizes(layer))
    for index <- 0 until layerSizes(layer) do 
      if layer == 0 then 
        neurons(layer)(index) = new Neuron(input)
      else
        neurons(layer)(index) = new Neuron(outputs(layer - 1))
  
  /** Walk through all neurons in all layers and forward outputs to next layer */
  def feedForward() = 
    for layer <- layerSizes.indices do
      for index <- outputs(layer).indices do
        outputs(layer)(index) = neurons(layer)(index).output()

  /** Use signal as input and feeding forward to output of last layer. */
  def predict(signal: Vec): Vec =
    for i <- input.indices do input(i) = signal(i)
    feedForward()
    outputs(lastLayer)
  
  /** Show this network with its neurons in each layer */
  def show: String = 
    val heading = s"Neural Network [inputSize=$inputSize, layerSizes=$layerSizes]"
    var body = 
      (
        for layer <- 0 until layerSizes.length yield
          s"Layer $layer: " ++ (
            for index <- 0 until layerSizes(layer) 
            yield neurons(layer)(index).show
          ).mkString(", ")
      ).mkString("\n")
    s"$heading\n$body"

  def mutateRandomNeuron(learningFactor: Num): (Int, Int) = 
    val (l, i) = randomNeuron()
    neurons(l)(i).mutate(learningFactor)
    (l, i)
  
  def train(cycles: Int, data: DataSet, learningFactor: Num = 0.3): Unit = 

    def computeLoss(): Num = 
      var averageLoss = 0.0
      var i = 0
      while i < data.size do
        val loss = meanSquareError(predict(data.inputs(i)), data.correctOutputs(i))
        i += 1
        averageLoss = averageLoss + (loss - averageLoss)/i 
      end while
      averageLoss
    end computeLoss

    for cycle <- 1 to cycles do
      val loss1 = computeLoss()
      val (l, i) = mutateRandomNeuron(learningFactor)
      val loss2 = computeLoss()
      if cycle % (cycles / 10) == 0 then 
        println(s"cycle $cycle loss before mutation: $loss1 --- after mutation: $loss2")
      if loss2 < loss1
      then neurons(l)(i).save() 
      else neurons(l)(i).backtrack()

  end train
