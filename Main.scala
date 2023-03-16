package scai 

val trainingData: Array[Array[Array[Float]]] = Array(
                    //input     correct output
  Array(Array[Float](170,79), Array[Float](1.0))
) 

@main def run =
  val result = rnd()
  println(s"hello scai\n  result = $result")