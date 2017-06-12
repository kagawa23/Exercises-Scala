package miniproject2.src.main.scala

// Advanced Programming. Andrzej Wasowski. IT University
// To execute this example, run "sbt run" or "sbt test" in the root dir of the project
// Spark needs not to be installed (sbt takes care of it)

import Main.{Embedding, ParsedReview}
import org.apache.spark.ml.classification.MultilayerPerceptronClassifier
import org.apache.spark.ml.evaluation.MulticlassClassificationEvaluator
import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.ml.Pipeline
import org.apache.spark.rdd.RDD
import org.apache.spark.sql.{DataFrame, Dataset, Row, SparkSession}
import org.apache.spark.sql.types._
import org.apache.spark.ml.feature.Tokenizer
import org.apache.spark.ml.linalg.{DenseVector, Vectors}
import org.apache.spark.ml.tuning.CrossValidator
import org.apache.spark.sql.functions._

import scala.collection.mutable

object Main {

	type Embedding = (String, List[Double])
	type ParsedReview = (Integer, String, Double)

	org.apache.log4j.Logger getLogger "org" setLevel (org.apache.log4j.Level.WARN)
	org.apache.log4j.Logger getLogger "akka" setLevel (org.apache.log4j.Level.WARN)
	val spark: SparkSession = SparkSession.builder
		.appName("Sentiment")
		.master("local[4]").config(new SparkConf().set("spark.driver" +
		".allowMultipleContexts", "true"))
		.getOrCreate

	import spark.implicits._

	val reviewSchema = StructType(Array(
		StructField("reviewText", StringType, nullable = false),
		StructField("overall", DoubleType, nullable = false),
		StructField("summary", StringType, nullable = false)))

	/**
		* Method maps an input value from 1 to 5 to one from 1 to 3
		*
		* @param value a value in {1.0, 2.0, 3.0, 4.0, 5.0}
		* @return a value in {1.0, 2.0, 3.0}
		*/
	def mapOverallValue(value: Double): Double = {
		value match {
			case 1.0 => 1.0
			case 2.0 => 1.0
			case 3.0 => 2.0
			case 4.0 => 3.0
			case 5.0 => 3.0
		}
	}

	// Read file and merge the text and summary into a single text column
	def loadReviews(path: String): Dataset[ParsedReview] = {
		spark
			.read
			.schema(reviewSchema)
			.json(path)
			.rdd
			.zipWithUniqueId
			.map[(Integer, String, Double)] { case (row, id) => (id.toInt,
			s"${row getString 2} ${row getString 0}".replaceAll("""[\p{Punct}]""",
				""),
			mapOverallValue(row getDouble 1)) }
			.toDS
			.withColumnRenamed("_1", "id")
			.withColumnRenamed("_2", "text")
			.withColumnRenamed("_3", "overall")
			.as[ParsedReview]
	}

	// Load the GLoVe embeddings file
	def loadGlove(path: String): Dataset[Embedding] = {
		spark
			.read
			.text(path)
			.map {
				_ getString 0 split " "
			}
			.map(r => (r.head, r.tail.toList.map(_.toDouble))) // yuck!
			.withColumnRenamed("_1", "word")
			.withColumnRenamed("_2", "vec")
			.as[Embedding]
	}


	def getTokenized(filePath: String): DataFrame = {
		val reviews: Dataset[ParsedReview] = loadReviews(filePath)
		val tokenizer: Tokenizer = new Tokenizer().setInputCol("text").setOutputCol("words")
		// todo use flatMap instead of explode
		val tokenized: DataFrame = tokenizer.transform(reviews)
		tokenized.select("id", "overall", "words")
			.withColumn("word", explode(col("words")))
	}

	def main(args: Array[String]): Unit = {
		val sparkContext : SparkContext = spark.sparkContext
		val timestampStart: Long = System.currentTimeMillis

		val newTokenized: DataFrame = getTokenized("resources/Musical_Instruments_5_half" +
			".json")
		//    sparkContext.parallelize(newTokenized.collect(), 2)

		val glove: Dataset[Embedding] = loadGlove("resources/glove.6B/glove.6B" +
			".mini" +
			".txt")
		//    sparkContext.parallelize(glove.collect(), 2)

		// Joining two tables
		val fourColDataset: DataFrame = newTokenized.as("newTokenized").toDF()
			.select("id", "word", "overall")
			.join(glove.as("glove"), col("glove.word") === col("newTokenized.word"))
		//    sparkContext.parallelize(fourColDataset.collect(), 2)

		// Summing vectors per id
		val twoColDatasetSum: DataFrame = fourColDataset
			.select(col("id"), col("vec")).as[(Int, Array[Double])]
			.groupByKey(k => k._1)
			.mapGroups((k, it) => (k, it.map(_._2).toList.transpose.map(_.sum)))
			.withColumnRenamed("_1", "id")
			.withColumnRenamed("_2", "sum")
		//    sparkContext.parallelize(twoColDatasetSum.collect(), 2)

		// Computing vector size
		val twoColDatasetSize: DataFrame = fourColDataset.select(col("id"), col("vec"), col
		("overall"))
			.as[(Int, Array[Double], Double)]
			.groupByKey(k => k._1)
			.mapGroups((k, it) => (k, it.size))
			.withColumnRenamed("_1", "id")
			.withColumnRenamed("_2", "size")
			.withColumnRenamed("_3", "overall")
			.toDF()
		//    sparkContext.parallelize(twoColDatasetSize.collect(), 2)

		val threeColDataset: RDD[Row] = twoColDatasetSum.as("sumDF").toDF()
			.select("id", "sum")
			.join(twoColDatasetSize.as("sizeDF"), col("sumDF.id") === col
			("sizeDF.id")).toJavaRDD
		//    sparkContext.parallelize(threeColDataset.collect(), 2)

		val averageColDataset: RDD[(String, List[Double])] = threeColDataset.map(x
		=> (x.get(0).toString,  x.get(1).asInstanceOf[mutable
			.WrappedArray[Double]].toList.map(element
			=> element / x.get(3).asInstanceOf[Int])))
		//    sparkContext.parallelize(averageColDataset.collect(), 2)

		val newTokenizedRDD: RDD[Row] = newTokenized.rdd
		//    sparkContext.parallelize(newTokenizedRDD.collect(), 2)

		val mappedFourColDataset: RDD[(String, Double)] = newTokenizedRDD.map(x
		=> (x.get(0).toString, x.get(1).asInstanceOf[Double]))
		//    sparkContext.parallelize(mappedFourColDataset.collect(), 2)

		val mappedFourColDatasetUnique: RDD[(String, Double)] =
			mappedFourColDataset.groupByKey().map(item => (item._1, item._2.toList
			(1)))
		//    sparkContext.parallelize(mappedFourColDatasetUnique.collect(), 2)

		val joinedAverageOverall: RDD[(String, (List[Double], Option[Double]))] =
			averageColDataset
				.leftOuterJoin(mappedFourColDatasetUnique)
		//    sparkContext.parallelize(joinedAverageOverall.collect(), 2)

		val joinedAverageOverallDS: DataFrame =
			joinedAverageOverall.map(item => (item._1, Vectors.dense(item._2._1.toArray), item._2._2)).toDS
				.withColumnRenamed("_1", "id")
				.withColumnRenamed("_2", "features")
				.withColumnRenamed("_3", "label")
		//    sparkContext.parallelize(joinedAverageOverallDS.collect(), 2)

		val splits = joinedAverageOverallDS.randomSplit(Array(0.9, 0.1), seed =
			1234L)
		//    sparkContext.parallelize(splits)
		val train = splits(0)
		val test = splits(1)
		// input layer of size 4 (features), two intermediate of size 5 and 4
		// and output of size 4 (highest predicted  category + 1)
		val numberOfFeatures: Int = joinedAverageOverallDS.first().get(1)
			.asInstanceOf[DenseVector].size
		val layers = Array[Int](numberOfFeatures, 5, 4, 4)
		// create the trainer and set its parameters
		val trainer: MultilayerPerceptronClassifier = new MultilayerPerceptronClassifier()
			.setLayers(layers)
			.setBlockSize(900)
			.setSeed(1234L)
			.setMaxIter(10)
		val pipeline: Pipeline = new Pipeline().setStages(Array(trainer))

		val evaluator: MulticlassClassificationEvaluator = new MulticlassClassificationEvaluator()
			.setMetricName("accuracy")
		val nFolds: Int = 10
		val cv = new CrossValidator()
			.setEstimator(pipeline)
			.setEstimatorParamMaps(Array(trainer.extractParamMap()))
			.setEvaluator(evaluator)
			.setNumFolds(nFolds)
		val model = cv.fit(train)
		// compute precision on the test set
		val result = model.transform(test)
		val predictionAndLabels = result.select("prediction", "label")

		println(s"Duration: ${(System.currentTimeMillis - timestampStart)} ms (~${(System.currentTimeMillis - timestampStart)/1000/60} minutes)")
		println("Precision:" + evaluator.evaluate(predictionAndLabels))

		spark.stop
	}
}
