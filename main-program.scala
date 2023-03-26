//> using scala "3.2.2"

object mainProgram:
  val welcomeMessage = "Welcome to AI SEX CLASSIFIER"

  import mathematics.* 

  /** Convert a number between 0 and 1 to a binary sex. */
  def binaryClassifier(x: Num): String = 
    if x > 0.5 
    then "Female" 
    else "Male  "

  val trainData = DataSet.fromFile("train-data.txt")
  val testData  = DataSet.fromFile("test-data.txt")

  val ai = new Network(inputSize = trainData.inputs(0).size, layerSizes = List(3,2,1))

  /** Show any text in color in terminal using for example colorCode=Console.RED */
  def showColor(text: String, colorCode: String): String = 
    colorCode + text + Console.RESET

  /** Use data to test our ai. A loss close to zero represents high certainty. **/
  def test(data: DataSet): Unit =
    for i <- data.inputs.indices do
      val predicted = ai.predict(data.inputs(i))
      val correct = data.correctOutputs(i)
      val loss = meanSquaredError(predicted, correct)

      val showPredicted = 
        if binaryClassifier(correct(0)) == binaryClassifier(predicted(0)) 
        then showColor(binaryClassifier(predicted(0)), Console.GREEN)
        else showColor(binaryClassifier(predicted(0)), Console.RED)

      println(
        s"${data.inputs(i).mkString(",")} " +
        s"correct=${binaryClassifier(correct(0))} ${correct.mkString(",")}  " +
        s"predicted=$showPredicted  ${predicted.mkString(",")} loss=$loss") 

  /** The main program. Click 'run' or type `scala-cli run .` in terminal. */
  @main def run = 
    println(s"\n--- $welcomeMessage\n")
    println(ai.show)
    val n = 600
    println(s"\n--- TRAINING in $n cycles")
    ai.train(steps = n,  data = trainData)
    println(s"\n--- TESTING")
    test(testData)

