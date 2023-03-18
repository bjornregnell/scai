package scai

class Network(nbrOfInputs: Int = 2, layerSizes: Seq[Int] = Seq(3,2,1)):

  val inputs = new Vec(nbrOfInputs)
  val outputs = new Array[Vec](layerSizes.length)
  val neurons = new Array[Array[Neuron]](layerSizes.length)

  def randomNeuron(): (Int, Int) = 
    val layer = util.Random.nextInt(layerSizes.length)
    val index = util.Random.nextInt(layerSizes(layer))
    (layer, index)

  val nbrOfNeurons = layerSizes.sum
  val lastLayer = layerSizes.length - 1
  
  // allocate neurons in layers
  for layer <- 0 until layerSizes.length do 
    outputs(layer) = new Vec(layerSizes(layer))
    neurons(layer) = new Array[Neuron](layerSizes(layer))
    for index <- 0 until layerSizes(layer) do 
      if layer == 0 then 
        neurons(layer)(index) = new Neuron(index, inputs, outputs(layer))
      else
        neurons(layer)(index) = new Neuron(index, outputs(layer - 1), outputs(layer))
  
  /** Walk through all neurons in all layers and forward outputs to next layer */
  def feedForward() = 
    for layer <- 0 until layerSizes.length do
      for index <- 0 until layerSizes(layer) do
        neurons(layer)(index).feedForward()

  def predict(input: Vec): Vec =
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

  def test(data: DataSet): Num =
    var averageLoss = 0.0
    var i = 0
    while i < data.size do
      val loss = meanSquareError(predict(data.inputs(i)), data.correctOutputs(i))
      def interpret(xs: Vec): String = if xs(0) > 0.5 then "Female" else "Male  "
      println(
        s"${data.inputs(i).mkString(",")} " +
        s"correct=${interpret(data.correctOutputs(i))} ${data.correctOutputs(i).mkString(",")}  " +
        s"predicted=${interpret(outputs(lastLayer))}  ${outputs(lastLayer).mkString(",")} loss=$loss") 
      i += 1 
      averageLoss = averageLoss + (loss - averageLoss)/i
    end while
    averageLoss
