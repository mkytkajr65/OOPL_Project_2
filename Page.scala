import scala.collection.mutable.ArrayBuffer
package PS
{
	class PageSummary(val url: String, val terms: List[String]){
		def fracMatching(term: String) : Double = {
			return terms.count(_ == term).toDouble/terms.length.toDouble
		}

		def fracMatching(termsToMatch: List[String]) : Double = {
			def getFrac() : Double = {
				var termToMatch = termsToMatch.head 
				var leftOverTerms = termsToMatch.tail

				var rv = terms.count(_ == termToMatch).toDouble/terms.length.toDouble + fracMatching(leftOverTerms)
				
				return rv
			}
			def baseCaseChecker(termsToMatchLength: Int) : Double = termsToMatchLength match {
				case l if l > 0  => getFrac()
				case _ => 0.0
			}
			return baseCaseChecker(termsToMatch.length)
		}
	}

	case class Page(val pageSummary: PageSummary)

	trait Weighted[A]{
		val items: Iterable[A]
		val weightingFn: A => Double

		def weights: Iterable[Double] = {
			for(item <- items) yield weightingFn(item)
		}

		def totalWeight: Double = {
			var sum = 0.0
			for(weight <- weights) {
				sum += weight
			}
			return sum
		}
		def sumIf(p: A => Boolean): Double = {
			var sum = 0.0
			for(item <- items) {
				if(p(item)){
					sum += weightingFn(item)
				}
			}
			return sum
		}
	}

	class IndexedPages(pages: ArrayBuffer[Page]) extends Iterable[Page]{
		override def iterator = pages.iterator

		val items = pages

		def search(q: Query) : SearchResults = {
			var rv = new SearchResults( q, this)
			return rv
		}

		def numContaining(word: String) : Double = {
			var count = 0
			for(page <- pages) {
				if(page.pageSummary.fracMatching(word) > 0.0){
					count += 1
				}
			}
			return count
		}
	}

	class WeightedIndexedPages(pages: ArrayBuffer[Page]) extends IndexedPages(pages: ArrayBuffer[Page]) with Weighted[Page]{

		val weightingFn = { (p: Page) =>  1.0/p.pageSummary.url.length.toDouble }

		override def search(q: Query) : SearchResults = {
			val beforeWeights: SearchResults = super.search(q)
			val oldScores = beforeWeights.results.unzip._1

			val unnormScores = oldScores.zip(weights).map { (x) => (x._1 * x._2) }

			//Multiplying by weights may change the scale of the scores
			//Thus, newScores is unnormScores by the total of the unnormScores
			// (This is called "normalizing" the scores)
			val total = unnormScores.foldLeft(0.0) {_+_}
			val newScores = unnormScores.map { _ / total }

			var rv = ArrayBuffer[(Double, String)]()

			var urlString = beforeWeights.results.unzip._2

			beforeWeights.results = newScores.zip(urlString)

			return beforeWeights
		}

		override def numContaining(word: String) : Double = {
			sumIf({ (page: Page) => (page.pageSummary.fracMatching(word) > 0.0) } )
		}
	}

	trait Augmentable[A] {
		val items: scala.collection.mutable.Seq[A] with scala.collection.generic.Growable[A]

		def add(newItem: A): Boolean = {
			if(items.count(newItem == _) > 0){
				return false
			}
			items += newItem
			return true
		}
	}

	class Query(terms: Iterable[String]){
		val items = terms.zipWithIndex

		val weightingFn = (x: (String, Int)) => 1.0
	}

	class WeightedQuery(terms: Iterable[String]) extends Query(terms: Iterable[String]) with Weighted[(String, Int)]{

		override val items = terms.zipWithIndex

		override val weightingFn = (x: (String, Int)) => (1.0/(x._2 + 3.0)).toDouble
	}

	class SearchResults(val query: Query, val ip: IndexedPages){
		var results = Iterable[(Double, String)]()


		def r() : Iterable[(Double, String)] = {
			var rv = ArrayBuffer[(Double, String)]()
			var indexedPagesCount = ip.items.length
			for(thisPage <- ip) {
				var IDF = 0.0
				var TF = 0.0
				var score = 0.0

				for(term <- query.items) {
					IDF = scala.math.log(indexedPagesCount/(ip.numContaining(term._1)))
					TF = thisPage.pageSummary.fracMatching(term._1)
					score += ((TF * IDF) * query.weightingFn(("", term._2)))
				}
				rv += ((score, thisPage.pageSummary.url))
			}
			return rv.sortWith( _._1 > _._1 )
		}

		results = r

		def printTop(n: Int): Unit = {
			var count = 0
			var searchResults = results
			for(sr <- searchResults) {
				if(count >= n){
					return
				}
				println(sr._1 + " " + sr._2)
				count += 1
			}
		}
	}

}