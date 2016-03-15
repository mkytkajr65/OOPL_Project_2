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

	class Page(val pageSummary: PageSummary){

		case class pageUrl(url: String)

		val page_url = pageUrl(pageSummary.url)
	}

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

		val weightingFn = { (p: Page) =>  1.0/p.page_url.url.length.toDouble }

		val items = pages

		override def numContaining(word: String) : Double = {
			sumIf({ (page: Page) => (page.pageSummary.fracMatching(word) > 0.0) } )
		}
	}
}