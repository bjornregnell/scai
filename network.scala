package scai

class Network(nbrOfInputs: Int = 2, layerSizes: Seq[Int] = Seq(3,2,1)):

  val inputs = new Array[Float](nbrOfInputs)
  val outputs = new Array[Array[Float]](layerSizes.length)
  val neurons = new Array[Array[Neuron]](layerSizes.length)

  def randomNeuron(): (Int, Int) = 
    val layer = util.Random.nextInt(layerSizes.length)
    val index = util.Random.nextInt(layerSizes(layer))
    (layer, index)

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
  
  /** Walk through all neurons in all layers and forward outputs to next layer */
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

  def mutateRandomNeuron(learningFactor: Float): (Int, Int) = 
    val (l, i) = randomNeuron()
    neurons(l)(i).mutate(learningFactor)
    (l, i)
  

  def train(iterations: Int, data: DataSet, learningFactor: Float = 0.3): Unit = 
    def computeLoss(): Float = 
      var averageLoss = 0F
      var i = 0
      while i < data.size do
        val loss = meanSquareLoss(predict(data.inputs(i)), data.correctOutputs(i))
        i += 1
        averageLoss += (loss - averageLoss)/i // https://en.wikipedia.org/wiki/Moving_average
      end while
      averageLoss
    end computeLoss

    for epoch <- 1 to iterations do
      val loss1 = computeLoss()
      val (l, i) = mutateRandomNeuron(learningFactor)
      val loss2 = computeLoss()
      if epoch % (iterations / 10) == 0 then 
        println(s"epoch $epoch loss before mutation: $loss1 --- after mutation: $loss2")
      if loss2 < loss1
      then neurons(l)(i).save() 
      else neurons(l)(i).backtrack()

  def test(data: DataSet): Float =
    var averageLoss = 0F
    var i = 0
    while i < data.size do
      val loss = meanSquareLoss(predict(data.inputs(i)), data.correctOutputs(i))
      def interpret(xs: Array[Float]): String = if xs(0) > 0.5 then "Female" else "Male  "
      println(
        s"${data.inputs(i).mkString(",")} " +
        s"correct=${interpret(data.correctOutputs(i))} ${data.correctOutputs(i).mkString(",")}  " +
        s"predicted=${interpret(outputs(lastLayer))}  ${outputs(lastLayer).mkString(",")} loss=$loss") 
      i += 1 
      averageLoss += (loss - averageLoss)/i // https://en.wikipedia.org/wiki/Moving_average
    end while
    averageLoss
