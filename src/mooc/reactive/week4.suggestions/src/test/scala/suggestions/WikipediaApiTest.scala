package suggestions



import language.postfixOps
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.{Try, Success, Failure}
import rx.lang.scala._
import org.scalatest._
import gui._
import org.junit.runner.RunWith
import org.scalatest.junit.JUnitRunner
import rx.lang.scala.concurrency.Schedulers


@RunWith(classOf[JUnitRunner])
class WikipediaApiTest extends FunSuite {

  object mockApi extends WikipediaApi {
    def wikipediaSuggestion(term: String) = Future {
      if (term.head.isLetter) {
        for (suffix <- List(" (Computer Scientist)", " (Footballer)")) yield term + suffix
      } else {
        List(term)
      }
    }
    def wikipediaPage(term: String) = Future {
      "Title: " + term
    }
  }

  import mockApi._

  test("WikipediaApi should make the stream valid using sanitized") {
    val notvalid = Observable("erik", "erik meijer", "martin")
    val valid = notvalid.sanitized

    var count = 0
    var completed = false

    val sub = valid.subscribe(
      term => {
        assert(term.forall(_ != ' '))
        count += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    assert(completed && count == 3, "completed: " + completed + ", event count: " + count)
  }

  test("WikipediaApi should correctly use concatRecovered") {
    val requests = Observable(1, 2, 3)
    val remoteComputation = (n: Int) => Observable(0 to n)
    val responses = requests concatRecovered remoteComputation
    val sum = responses.foldLeft(0) { (acc, tn) =>
      tn match {
        case Success(n) => acc + n
        case Failure(t) => throw t
      }
    }
    var total = -1
    val sub = sum.subscribe {
      s => total = s
    }
    assert(total == (1 + 1 + 2 + 1 + 2 + 3), s"Sum: $total")
  }
  
  test("WikipediaApi recovered") {
    val stream = Observable(1, 2, 3) ++ Observable(new NoSuchElementException)
    
    var successCount = 0
    var failCount = 0
    var completed = false
    
    val res = stream.recovered.subscribe(
      n => n match {
        case Success(v) => successCount += 1
        case Failure(e) => failCount += 1
      },
      t => assert(false, s"stream error $t"),
      () => completed = true
    )
    
    assert(completed && successCount == 3 && failCount == 1, s"success count: $successCount, fail count: $failCount")
  }
  
  test("WikipediaApi timedOut") {
    // terminates before initial stream ends
    assert(Observable.interval(100 milliseconds).take(31).timedOut(3).toBlockingObservable.toList.length == 30)
    
    // terminates first if < specified time out value
    assert(Observable.interval(100 milliseconds).take(10).timedOut(3).toBlockingObservable.toList.length == 10)
    
    // TODO need to check for exception
  }
  
  // Tests found in the discussion forum
  
  test("concatRecovered test with delays to test correct ordering") {
    val requests = Observable(3, 2, 1)
    val remoteComputation = (num: Int) => Observable.interval(num seconds).map(_ => num).take(2)
    val responses = requests concatRecovered remoteComputation
    val result = responses.toBlockingObservable.toList
    assert (result == List(Success(3), Success(3),
      Success(2), Success(2),
      Success(1), Success(1)))
  }

  test("concatRecovered behaves as promised") {
    val req = Observable(1,2,3,4,5)
    val response = req.concatRecovered(num => if (num != 4) Observable(num) else Observable(new Exception))

    val res = response.foldLeft((0,0)) { (acc, tn) =>
      tn match {
        case Success(n) => (acc._1 + n, acc._2)
        case Failure(_) => (acc._1, acc._2 + 1)
      }
    }

    var pair = (0, 0)
    res.observeOn(Schedulers.immediate).subscribe(e => pair = e)
    val (sum, fc) = pair
    assert(sum == (1 + 2 + 3 + 5), "Wrong sum: " + sum)
    assert(fc == 1, "Wrong failurecount: " + fc)
  }
}