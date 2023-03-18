package scai 

class DataSet(val inputs: Seq[Array[Float]], val correctOutputs: Seq[Array[Float]]):
  assert(inputs.length == correctOutputs.length, "inputs.length must be same as correctOutput.length")
  def size = inputs.length
  override def toString = s"${super.toString}[size: $size rows, first row length: ${inputs(0).length}]"

object DataSet:
  def fromFile(file: String, enc: String = "UTF-8"): DataSet =
    val source = scala.io.Source.fromFile(file, enc)
    val inputs = List.empty[Array[Float]].toBuffer
    val correctOutputs = List.empty[Array[Float]].toBuffer
    for line <- source.getLines() do
      val xs: Array[String] = line.split(":")
      val is: Array[Float] = xs.lift(0).getOrElse("").split(",").map(_.toFloatOption.getOrElse(0F))
      val os: Array[Float] = xs.lift(1).getOrElse("").split(",").map(_.toFloatOption.getOrElse(0F))
      inputs.append(is)
      correctOutputs.append(os)
    end for
    new DataSet(inputs.toSeq, correctOutputs.toSeq)

val trainingDataFile = "training-data.txt"
val trainingData = 
  println(s"\n---TRAINING DATA from file $trainingDataFile")
  DataSet.fromFile(trainingDataFile)

val testingDataFile = "testing-data.txt"
val testingData = 
  println(s"---TESTING DATA  from file $testingDataFile")
  DataSet.fromFile(testingDataFile)

val network = Network(trainingData.inputs(0).length)

def trainAndTest() =
  println(network.show)
  println("\nTraining...")
  network.train(600,  data = trainingData)
  println("\nTesting...")
  val loss = network.test(testingData)
  println(s"Average loss in testing: $loss")  

@main def run = trainAndTest()

