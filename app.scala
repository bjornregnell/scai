//> using scala "3.3.0-RC3"

package scai 

class DataSet(name: String, val inputs: Seq[Vec], val correctOutputs: Seq[Vec]):
  def size = inputs.size
  override def toString = s"DataSet $name size: $size rows, first row length: ${inputs(0).length}]"

object DataSet:
  def fromFile(file: String, enc: String = "UTF-8"): DataSet =
    val source = scala.io.Source.fromFile(file, enc)  // a source can read lines from a file
    val inputs = List.empty[Vec].toBuffer // a buffer is a list that can grow
    val correctOutputs = List.empty[Vec].toBuffer
    for line <- source.getLines() do
      val strings: Array[String] = line.split(":")
      val is: Vec = strings(0).split(",").map(_.toDouble)
      val os: Vec = strings(1).split(",").map(_.toDouble)
      inputs.append(is)
      correctOutputs.append(os)
    end for
    new DataSet(file, inputs.toSeq, correctOutputs.toSeq)

val trainFile = "train-data.txt"
val trainData = DataSet.fromFile(trainFile)

val testFile = "test-data.txt"
val testData = DataSet.fromFile(testFile)

val network = Network(trainData.inputs(0).size)

def trainAndTest() =
  println(network.show)
  println(s"  Training with $trainFile")
  network.train(600,  data = trainData)
  println(s"  Testing with $testFile")
  val loss = network.test(testData)
  println(s"  Average loss in testing: $loss")  

@main def run = trainAndTest()

