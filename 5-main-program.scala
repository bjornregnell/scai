//> using scala "3.5.1"

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

end mainProgram
