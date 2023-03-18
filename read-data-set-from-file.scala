class DataSet(name: String, val inputs: Seq[Vec], val correctOutputs: Seq[Vec]):
  def size = inputs.size
  override def toString = s"DataSet $name size: $size rows, first row length: ${inputs(0).length}]"

object DataSet:
  def fromFile(file: String, enc: String = "UTF-8"): DataSet =
    val source = scala.io.Source.fromFile(file, enc)  // a source can read lines from a file
    val inputs = List.empty[Vec].toBuffer // a buffer is a list that can grow
    val correctOutputs = List.empty[Vec].toBuffer
    for line <- source.getLines() if line.nonEmpty do
      val strings: Array[String] = line.split(":")
      val x = strings(0).split(",").map(_.toDouble)
      val y = strings(1).split(",").map(_.toDouble)
      inputs.append(x)
      correctOutputs.append(y)
    end for
    new DataSet(file, inputs.toSeq, correctOutputs.toSeq)