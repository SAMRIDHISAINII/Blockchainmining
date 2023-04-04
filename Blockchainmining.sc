import java.security.MessageDigest
import scala.util.control.Breaks._

object Blockchain {
  val MAX_NONCE = 100000000000L

  def SHA256(text: String): String = {
    val md = MessageDigest.getInstance("SHA-256")
    val hash = md.digest(text.getBytes("UTF-8"))
    hash.map("%02x".format(_)).mkString
  }

  def mine(blockNumber: Int, transactions: String, previousHash: String, prefixZeros: Int): String = {
    val prefixStr = "0" * prefixZeros
    var nonce = 0L
    breakable {
      for (nonce <- 0L to MAX_NONCE) {
        val text = blockNumber.toString + transactions + previousHash + nonce.toString
        val newHash = SHA256(text)
        if (newHash.startsWith(prefixStr)) {
          println(s"Yay! Successfully mined bitcoins with nonce value: $nonce")
          break
        }
      }
      if (nonce >= MAX_NONCE) {
        throw new RuntimeException(s"Couldn't find correct hash after trying $MAX_NONCE times")
      }
    }
    SHA256(blockNumber.toString + transactions + previousHash + nonce.toString)
  }

  def main(args: Array[String]): Unit = {
    val transactions =
      """
        |Dhaval->Bhavin->20,
        |Mando->Cara->45
        |""".stripMargin
    val difficulty = 4
    val start = System.currentTimeMillis()
    println("start mining")
    val newHash = mine(5, transactions, "0000000xa036944e29568d0cff17edbe038f81208fecf9a66be9a2b8321c6ec7", difficulty)
    val total_time = (System.currentTimeMillis() - start) / 1000.0
    println(s"end mining. Mining took: $total_time seconds")
    println(newHash)
  }
}
