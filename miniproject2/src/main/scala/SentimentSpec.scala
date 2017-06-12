//import miniproject2.src.main.scala.Main
//import org.apache.spark.SparkContext
//import org.apache.spark.sql._
//import org.scalatest.{BeforeAndAfterAll, _}
//
//import scala.collection.mutable
//
//class SentimentSpec extends FlatSpec with BeforeAndAfterAll {
//
//  org.apache.log4j.Logger getLogger "org" setLevel (org.apache.log4j.Level.WARN)
//  org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
//
//  val spark: SparkSession = SparkSession.builder
//    .appName("Sentiment")
//    .master("local[3]")
//    .getOrCreate
//
//  override def afterAll: Unit = spark.stop
//
//  private val pathToGlove = "resources/glove.6B/glove.6B.mini.txt"
//  val glove: Dataset[(String, List[Double])] = Main.loadGlove(pathToGlove)
//  val pathToReviews: String = "resources/reviews_Musical_Instruments_5_mini.json"
//
//  "getTokenized" should "throw AnalysisException for invalid paths" in {
//    val invalidPathToReviews: String = "resources/no_file"
//    assertThrows[AnalysisException] {
//      Main.getTokenized(invalidPathToReviews)
//    }
//  }
//
//  "getTokenized" should "return expected values in the file" in {
//    val columnIndexOverall: Int = 1
//    val overallExpectedValue: Double = 3.0
//    assert(Main.getTokenized(pathToReviews).first().get(columnIndexOverall) == overallExpectedValue)
//    val columnIndexSummary: Int = 3
//    val summaryExpectedValue: String = "good"
//    assert(Main.getTokenized(pathToReviews).first().get(columnIndexSummary) == summaryExpectedValue)
//  }
//
//  val newTokenized: DataFrame = Main.getTokenized(pathToReviews)
//
////  "joinGloveWithTokenizedReviews" should "return expected values in the " +
////    "joined dataframe" in {
////    val gloveWithReviews: DataFrame = Main
////      .joinGloveWithTokenizedReviews(newTokenized, glove)
////    val expectedWordEmbeddingSize: Int = 50
////    val firstWordEmbedding: mutable.WrappedArray[String] = gloveWithReviews.first().get(4)
////      .asInstanceOf[mutable.WrappedArray[String]]
////    assert(firstWordEmbedding.size == expectedWordEmbeddingSize)
////    val firstWordInReview: String = "not"
////    assert(gloveWithReviews.first().get(1) == firstWordInReview)
////
////    glove.createOrReplaceTempView("glove")
////    val sparkContext: SparkContext = spark.sparkContext
////    val sqlContext = new org.apache.spark.sql.SQLContext(sparkContext)
////    val expectedWordEmbedding: mutable.WrappedArray[String] = sqlContext.sql("select * from glove where word='not'").first().get(1).asInstanceOf[mutable.WrappedArray[String]]
////    assert(expectedWordEmbedding.toList.equals(firstWordEmbedding.toList))
////  }
//
////  "summedVectors" should "return expected values" in {
////    val gloveWithReviews: DataFrame = Main
////      .joinGloveWithTokenizedReviews(newTokenized, glove)
////    gloveWithReviews.createOrReplaceTempView("gloveWithReviews")
////    newTokenized.createOrReplaceTempView("newTokenized")
////    val sparkContext: SparkContext = spark.sparkContext
////    val sqlContext = new org.apache.spark.sql.SQLContext(sparkContext)
////    val twoColDatasetSum: DataFrame = summedVectors(gloveWithReviews)
////
////    assert(sqlContext.sql("select id, count(id) from gloveWithReviews group " +
////      "by id").first().get(0) == 1)
////
////    val expectedID: String = sqlContext.sql("select id, count(id) from " +
////      "gloveWithReviews group by id").first().get(0).toString
////    val actualID: String = twoColDatasetSum.first().get(0).toString
////    assert(actualID == expectedID)
////  }
//
//
//}
