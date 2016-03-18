import PS._
import org.apache.http._
import org.apache.http.client.entity._
import org.apache.http.client.methods._
import org.apache.http.impl.client._
import org.apache.http.client.utils._
import org.apache.http.message._
import org.apache.http.params._
import java.net.URL
import scala.collection.mutable.ArrayBuffer
import collection.breakOut
import collection.mutable
object SearchEngine extends App {
// body of our App is like main

	def fetch(url:String): String = {
	    val httpClient = new DefaultHttpClient()
	    var responseBody : String = ""
	    try { 
	      responseBody = httpClient.execute(new HttpGet(url),new BasicResponseHandler())
	    } catch {
	      case e: Exception => println(e.getMessage)
	    }

    return responseBody
  }

	def getLinks( html : String , baseURL : String) : List[String] = {
	// See http://www.mkyong.com/regular-expressions/how-to-extract-html-links-with-regular-expression/ for explanation of regex
		val aTagRegex = """(?i)<a([^>]+)>(.+?)</a>""".r
		val urlRegex = """\s*(?i)href\s*=\s*(\"([^"]*\")|'[^']*'|([^'">\s]+))""".r

		val opts = for ( a <- aTagRegex findAllMatchIn html ) yield urlRegex.findFirstMatchIn(a.toString)

		val hrefs = opts collect { case Some(x) => x group 1 }

		// remove leading and trailing quotes, if any
		val cleaned = hrefs map { _.stripPrefix("\"").stripPrefix("\'").stripSuffix("\"").stripPrefix("\'") } filter { ! _.startsWith("javascript") }

		// Use Java's URL class to parse the URL
		//   and get the full URL string (including implied context)
		val contextURL = new java.net.URL(baseURL)

		def getURL(x: String) = {
          var result = ""
          try {
            result = new java.net.URL(contextURL, x).toString()
          }
          catch {
            case e: java.net.MalformedURLException => Unit
          }
          result
        }
        
        (cleaned map { getURL(_) } ).filter(_.length > 0).toList
	}

	def getTerms(html: String, f: (String) => Boolean) : List[String] = {
		var regexMatchedList = html.split("""[^/sa-zA-z0-9]|[_//]+""").toList.filter(!_.isEmpty)
		regexMatchedList = for(rl  <- regexMatchedList ; if(f(rl))) yield rl //for-comprehension yield statement
		return regexMatchedList
	}

	def printBest(query : List[String], pages : List[PageSummary]) = {
		val scores = for(x <- pages) yield (x.url, x.fracMatching(query))
		for (x <- scores.sortBy(_._2).takeRight(5).reverse) println(x._1 + ": " + x._2.toString)
	}

	var counter = 0

	def getAllPages(linksOnPage: Set[String], html: String, maxPages: Int, prevUrls : scala.collection.mutable.Set[String], url: String) : ArrayBuffer[Page] = {
				var pageList = ArrayBuffer[Page]()

				//iterate over all the links and recursively crawl those links
				for(link  <- linksOnPage) {
				 	pageList ++= crawlAndIndex(link, maxPages, prevUrls)
				}
				//get all the terms who have a length greater than 1
				var keyTerms = getTerms(html, { (x:String) => (x.length>1)})
				//get the page summary of the current page
				var pageSummary = new PageSummary(url, keyTerms)
				//create new page
				var page = new Page(pageSummary)
				//append the pageSummary of the current page to the pageSummaryList

				var allPages = pageList += page

				return allPages
	}

	def crawlAndIndex(url: String, maxPages: Int, prevUrls : scala.collection.mutable.Set[String] = scala.collection.mutable.Set(), mode: String = "read"
		, weight: Boolean = true): IndexedPages = {
		//if there are pages left get all the page summaries for the urls on the page
		counter += 1
		if(maxPages>=counter){
			//println(counter)
			//println("url: " + url + " counter: " + counter)
			//fetch html
			var html = fetch(url) 
			//grab all the links on the page
			var linksOnPage = getLinks(html,url).toSet
			//make sure their are no repeats
			prevUrls += url
			
			 //.filter({ !prevUrls.contains(_) })
			//println(linksOnPage + " counter: " + counter)
			if(weight == true){
	
				var allPages = getAllPages(linksOnPage, html, maxPages, prevUrls, url)
			
				if(mode == "augmented"){
					var weightedAugmentedIndexPSL = new WeightedIndexedPages(allPages) with Augmentable[Page]
					return weightedAugmentedIndexPSL
				}
				var weightedIndexPSL = new WeightedIndexedPages(allPages)
				return weightedIndexPSL

				}else{
					
					var allPages = getAllPages(linksOnPage, html, maxPages, prevUrls, url)

					if(mode == "augmented"){
						var indexedAugmentedIndexPSL = new IndexedPages(allPages) with Augmentable[Page]
						return indexedAugmentedIndexPSL
					}
					var indexedPages = new IndexedPages(allPages)

					return indexedPages
				}
		}
		var emptyPageList = ArrayBuffer[Page]()
		var returnEmptyPageList = new IndexedPages(emptyPageList)
		return returnEmptyPageList
	}

	/*val searchList = List("plaza","shopping","shop","jacksonville","the","div")*/

	var u = "http://www.jacksonvilleplaza.com/"
	/*var results = crawlAndIndex(u,50)

	printBest(searchList,results)*/

	/*val ps = new PageSummary(u,searchList)

	val aPage = new Page(ps)

	var pages = ArrayBuffer[Page](aPage)*/

	//var results = crawlAndIndex(u,3)

	//println(results.map({ _.pageSummary.url }))

	/*var t = ArrayBuffer("Jacksonville")

    var query = new Query(t)

	var searchResults = new SearchResults(query, crawlAndIndex(u, 4))

	print(searchResults.results)
	searchResults.printTop(2)*/

	val pages = SearchEngine.crawlAndIndex("http://www.hotnewhiphop.com/", 100, weight=false)

	//val q = new WeightedQuery(List("computer", "security", "tech"))

	//pages.search(q).printTop(10)

	//val q2 = new Query(Vector("winter","storm"))

	//pages.search(q2).printTop(5)
}