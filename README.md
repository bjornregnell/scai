# Play with Artificial Intelligence (AI)

Have you ever wondered how AI works? 

Here you find an example of a very simple AI in the form of a small neural network implemented in the beginner-friendly Scala programming language. 

* This AI has 6 neurons with in total 20 parameters that are adjusted during training on 4 data points. 
* A smart AI such as ChatGPT has more than 100 million parameters and is trained on really big data...

See also slides from my talk here: 
  * https://github.com/bjornregnell/AI-inside

## How to run

You can run this code **online** here: https://scastie.scala-lang.org/HF8a4GJMQiaLRyx712KQpg

Or you can run it **locally** on your own machine as follows:

* Install latest Scala (>3.5) from here: https://www.scala-lang.org/download/

* Download this [zip-file](https://github.com/bjornregnell/scai/archive/refs/heads/main.zip) and unpack it. 

* Open a terminal [(see here how to open a terminal)](https://www.youtube.com/results?search_query=how+to+open+terminal) and navigate to the folder where you unpacked the zip-file, with some command similar to `cd Downloads/scai-main` and then run this command:

  ```
  scala run .
  ```

## Prototype network visualization

This prototype web app visualizes the network: https://eryndir.github.io/vscAi/ 

The prototype visualization code is available here: https://github.com/Eryndir/vscAi


## The Code, 230 lines of code in total

```scala
//> using scala 3.8.2

val welcomeMessage = "Welcome to AI SEX CLASSIFIER"

object mathematics:
  /** A decimal number with double precision */
  type Num = Double

  /** A vector with many numbers */
  type Vec = Array[Num]

  def multiply(x: Vec, y: Vec): Num =
    var result = 0.0
    for i <- x.indices do 
      result = result + x(i) * y(i)
    end for
    result

  def meanSquaredError(correct: Vec, predicted: Vec): Num =
    var sumOfSquares = 0.0
    for i <- correct.indices do
      val error = correct(i) - predicted(i)
      sumOfSquares = sumOfSquares + error * error
    end for
    sumOfSquares / correct.size

  /** An S-shaped function that scales the input to a number between 0.0 and 1.0
    * https://en.wikipedia.org/wiki/Sigmoid_function **/
  inline def sigmoid(x: Num): Num = (1 / (1 + math.exp(-x)))

  /** A Random Number Generator*/
  val RNG = new java.util.Random()

  /** A random number with normal distribution, mean 0, standard deviation 1 **/ 
  def random(): Num = RNG.nextGaussian()

end mathematics

export mathematics.* 

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
  
  def show: String = s"Neuron[${input.size} inputs]"
end Neuron

/** A simple model of a brain with neurons in layers. */
class Network(val inputSize: Int, val layerSizes: IArray[Int]):
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

class DataSet(val inputs: Array[Vec], val correctOutputs: Array[Vec]):
  def size = inputs.size
  require(size == correctOutputs.size)

object DataSet:
  /** Create a data set from a multi-line string.*/
  def fromLines(multiLineString: String): DataSet =
    val lines = multiLineString.trim.split("\n").map(_.trim)
    val pairs = lines.map(_.split(":"))
    val inputs: Array[Vec]  = pairs.map(p => p(0).split(",").map(_.toDouble))
    val correct: Array[Vec] = pairs.map(p => p(1).split(",").map(_.toDouble))
    new DataSet(inputs, correct)
end DataSet

/** Convert a number between 0 and 1 to a binary sex. */
def binaryClassifier(x: Num): String = 
  if x > 0.5 
  then "Female" 
  else "Male  "

val trainData = DataSet.fromLines:
  """167,73:0
     105,67:1
     120,72:1
     143,67:0
     130,66:0"""
     
val testData  = DataSet.fromLines:
  """115,66:1
     175,78:0
     205,72:0
     120,67:1"""

val ai = new Network(inputSize = trainData.inputs(0).size, layerSizes = IArray(3,2,1))

/** Show any text in color in terminal using for example colorCode=Console.RED */
  def showColor(s: String, colorCode: String): String = colorCode + s + Console.RESET

/** Use data to test our ai. An error close to zero represents high certainty. **/
def test(data: DataSet): Unit =
  for i <- data.inputs.indices do
    val predicted = ai.predict(data.inputs(i))
    val correct = data.correctOutputs(i)
    val error = meanSquaredError(predicted, correct)

    val predictedSex = binaryClassifier(predicted(0))
    val correctSex   = binaryClassifier(correct(0))
    
    val showPredicted = 
      if predictedSex == correctSex 
      then showColor(predictedSex, Console.GREEN)
      else showColor(predictedSex, Console.RED)

    println(
      s"${data.inputs(i).mkString(",")} " +
      s"correct=$correctSex ${correct.mkString(",")}  " +
      f"predicted=$showPredicted  ${predicted(0)}%1.10f") 

/** The main program. Click 'run' or type `scala run .` in terminal. */
@main def run = 
  println(s"\n====  $welcomeMessage  ====\n")
  println(ai.show)
  val n = 600

  println(s"\n--- TRAINING in $n steps")
  ai.train(steps = n,  data = trainData)
    
  println(s"\n--- TESTING")
  test(testData)

```

