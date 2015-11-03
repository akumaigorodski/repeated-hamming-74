import scala.language.postfixOps
import scala.util.Random
import breeze.linalg._
import GF2Semirings._
import Code74._

object Hello {
  def main( args: Array[String] ): Unit = {
    val message = "We have to embrace pain and burn it as fuel for our journey"
    val repeatTimes = 9
    
    // Convert 4-bit chunks into redundant 7-bit hamming code vectors and repeat <repeatTimes> times
    val repeatedEncodedBitMessage = encodeWithRepetition(message, repeatTimes).toList
    
    // Simulate severe noisy channel with 20% bit flip probability
    val repeatedEncodedNoisyBitMessage = for (v <- repeatedEncodedBitMessage) yield if (Random.nextInt(100) < 20) v + One else v
	
    // Decode recieved message and print it out
    val res = repeatedEncodedNoisyBitMessage grouped repeatTimes map majorityVote grouped 7 map Seq2Vec
    (bits2Chars andThen (_.mkString) andThen println)(res map doCheck map (Reverse * _) flatMap (_.toArray) toList)
  }
}
