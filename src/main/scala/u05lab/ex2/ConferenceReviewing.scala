package u05lab.ex2

import u05lab.ex2.ConferenceReviewing.Question

import java.util.stream.Collectors
import scala.collection.mutable.ListBuffer

trait ConferenceReviewing:

  def loadReview(article: Int, scores: Map[Question, Int]): Unit

  def loadReview(article: Int, relevance: Int, significance: Int,confidence: Int, fin: Int): Unit

  def orderedScores(article: Int, question: Question): List[Int]

  def averageFinalScore(article: Int): Double

  def acceptedArticles: Set[Int]

  def sortedAcceptedArticles: List[(Int, Double)]

  def averageWeightedFinalScoreMap: Map[Int, Double]


object ConferenceReviewing:

  enum Question:
    case RELEVANCE, SIGNIFICANCE, CONFIDENCE, FINAL

  def apply(): ConferenceReviewing = new ConferenceReviewingImpl

  private class ConferenceReviewingImpl extends ConferenceReviewing:

    private var reviews: List[(Int, Map[Question, Int])] = Nil

    override def loadReview(articleId: Int, scores: Map[Question, Int]): Unit =
      reviews = reviews.++(List((articleId, scores)))

    override def loadReview(articleId: Int, relevance: Int, significance: Int, confidence: Int, fin: Int): Unit =
      reviews = reviews.++(List((articleId, Map(Question.RELEVANCE -> relevance, Question.SIGNIFICANCE -> significance, Question.CONFIDENCE -> confidence, Question.FINAL -> fin))))

    override def orderedScores(article: Int, question: Question): List[Int] =
      reviews.filter(_._1 == article).map(_._2(question)).sorted

    override def averageFinalScore(article: Int): Double =
      reviews.filter(_._1 == article).map(_._2(Question.FINAL)).sum / reviews.count(_._1 == article).toDouble

    override def acceptedArticles: Set[Int] =
      reviews.map(_._1).distinct.filter(accepted).toSet

    override def sortedAcceptedArticles: List[(Int, Double)] =
      acceptedArticles.map(x => (x, averageFinalScore(x))).toList.sorted((x,y) => x._2.compareTo(y._2))

    override def averageWeightedFinalScoreMap: Map[Int, Double] =
      reviews.map(_._1).distinct.map(x => x -> averageWeightedFinalScore(x)).toMap

    private def accepted(article: Int): Boolean =
      averageFinalScore(article) > 5.0 && reviews.filter(_._1 == article).map(x => x._2).count(_ (Question.RELEVANCE) >= 8) != 0

    private def averageWeightedFinalScore(article: Int): Double =
      reviews.filter(_._1 == article).map[Double]( x => x._2(Question.FINAL) * (x._2(Question.CONFIDENCE) / 10.0 )).sum / reviews.count(_._1 == article).toDouble
