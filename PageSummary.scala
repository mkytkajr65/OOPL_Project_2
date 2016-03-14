package PS
{

	class PageSummary(val url: String, val terms: List[String]){
		def fracMatching(term: String) : Double = {
			return terms.count(_ == term)/terms.length
		}

		def fracMatching(termsToMatch: List[String]) : Double = {
			def getFrac() : Double = {
				var termToMatch = termsToMatch.head 
				var leftOverTerms = termsToMatch.tail

				var rv = terms.count(_ == termToMatch).toDouble/terms.length + fracMatching(leftOverTerms)
				
				return rv
			}
			def baseCaseChecker(termsToMatchLength: Int) : Double = termsToMatchLength match {
				case l if l > 0  => getFrac()
				case _ => 0.0
			}
			return baseCaseChecker(termsToMatch.length)
		}
	}
}