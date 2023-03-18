//> using scala "3.2.2"

val welcomeMessage = "Welcome to AI SEX CLASSIFIER"

val trainData = DataSet.fromFile("train-data.txt")
val testData  = DataSet.fromFile("test-data.txt")

def binaryClassifier(x: Num): String = 
  if x > 0.5 
  then "Female" 
  else "Male  "

val ai = new Network(inputSize = trainData.inputs(0).size, layerSizes = List(3,2,1))

def showColor(s: String, color: String): String = color + s + Console.RESET

def test(data: DataSet): Unit =
  for i <- data.inputs.indices do
    val predicted = ai.predict(data.inputs(i))
    val correct = data.correctOutputs(i)
    val loss = meanSquareError(predicted, correct)

    val showPredicted = 
      if binaryClassifier(correct(0)) == binaryClassifier(predicted(0)) 
      then showColor(binaryClassifier(predicted(0)), Console.GREEN)
      else showColor(binaryClassifier(predicted(0)), Console.RED)

    println(
      s"${data.inputs(i).mkString(",")} " +
      s"correct=${binaryClassifier(correct(0))} ${correct.mkString(",")}  " +
      s"predicted=$showPredicted  ${predicted.mkString(",")} loss=$loss") 

@main def run = 
  println(s"\n--- $welcomeMessage\n")
  println(ai.show)
  val n = 600
  println(s"\n--- TRAINING in $n cycles")
  ai.train(cycles = n,  data = trainData)
  println(s"\n--- TESTING")
  test(testData)

