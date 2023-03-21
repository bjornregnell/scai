class DataSet(val inputs: Array[Vec], val correctOutputs: Array[Vec]):
  def size = inputs.size
  require(size == correctOutputs.size)

object DataSet:
  /** Create a data set from file. */
  def fromFile(file: String, enc: String = "UTF-8"): DataSet =
    val source = scala.io.Source.fromFile(file, enc) 
    try fromLines(source.getLines().mkString("\n")) finally source.close

  /** Create a data set from a multi-line string.*/
  def fromLines(multiLineString: String): DataSet =
    val lines = multiLineString.trim.split("\n")
    val pairs = lines.map(_.split(":"))
    val inputs: Array[Vec]  = pairs.map(p => p(0).split(",").map(_.toDouble))
    val correct: Array[Vec] = pairs.map(p => p(1).split(",").map(_.toDouble))
    new DataSet(inputs, correct)