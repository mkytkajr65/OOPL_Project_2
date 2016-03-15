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

	def crawlAndIndex(url: String, maxPages: Int) : List[PageSummary] = {
		//if there are pages left get all the page summaries for the urls on the page
		counter += 1
		if(maxPages>=counter){
			println(counter)
			println("url: " + url + " maxpages: " + maxPages)
			//fetch html
			var html = fetch(url) 
			//grab all the links on the page
			var linksOnPage = getLinks(html,url)
			//make sure their are no repeats
			linksOnPage = linksOnPage.distinct
			var pageSummaryList : List[PageSummary] = Nil
			//iterate over all the links and recursively crawl those links
			for(link  <- linksOnPage) {
			 	pageSummaryList :::= crawlAndIndex(link, maxPages)  
			}
			//get all the terms who have a length greater than 1
			var keyTerms = getTerms(html, { (x:String) => (x.length>1)})
			//get the page summary of the current page
			var pageSummary = new PageSummary(url, keyTerms)
			//append the pageSummary of the current page to the pageSummaryList
			var allPages = pageSummaryList :+ pageSummary
			//make sure no duplicate pages summaries
			allPages = allPages.groupBy(_.url).map(_._2.head)(breakOut)

			return allPages
		}
		return List[PageSummary]()
	}

	val searchList = List("plaza","shopping","shop","jacksonville","the","div")

	var u = "http://www.jacksonvilleplaza.com/"
	/*var results = crawlAndIndex(u,50)

	printBest(searchList,results)*/

	val ps = new PageSummary(u,searchList)

	val aPage = new Page(ps)

	var pages = ArrayBuffer[Page](aPage)

	var ip = new WeightedIndexedPages(pages)

	println(ip.numContaining("shop"))

}