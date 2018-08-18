package practice

import play.api.libs.json._
import scala.util._

object Freenome extends App {

  type JsMap = scala.collection.Map[String, JsValue]

  val conf = Json.parse("""
    {
        "serviceA": {
            "host": "localhost",
            "port": 3030
        },
        "serviceB": {
            "host": "0.0.0.0",
            "port": 4040,
            "db": {
                "hostname": "dbhost", 
                "dbname": "mydb",
                "port": 3452
            }
        },
        "serviceC": {
            "featureFlags": {
                "feature1": true,
                "feature2": false,
                "feature3": true
            }
        },
        "generalThingy": "hello"
    }""")

  /* Converts a JsValue into a Map of [String, JsValue] if possible */
  def safeCastToMap(a: JsValue): Option[JsMap] = {
    a match {
      case JsObject(map) => Some(map)
      case _             => None
    }
  }

  /* Gets configuration key */
  def get(conf: JsValue, key: String): Option[JsValue] = {
    val tokens = key.split("\\.").toList

    def findKey(xs: List[String], submap: JsMap): Option[JsValue] = {
      xs match {
        case x :: Nil => submap.get(x)
        case x :: xs  => submap.get(x).flatMap(safeCastToMap).flatMap(findKey(xs, _))
        case _        => None
      }
    }
    safeCastToMap(conf).flatMap(m => findKey(tokens, m))
  }

  /* Checks if the JsValue object is a Map */
  def isLeaf(entry: (String, JsValue)): Boolean = {
    safeCastToMap(entry._2).isEmpty
  }

  /* List all keys in the configuration */
  def listKeys(conf: JsValue, prefixes: Seq[String] = Seq()): Seq[String] = {

    /* builds the key name*/
    def buildKey(x: String) = {
      (prefixes :+ x).mkString(".")
    }

    /* appends unresolved nodes, recurses */
    def appendNodes(xs: Seq[String], entry: (String, JsValue)): Seq[String] = {
      val (key, value) = entry
      xs ++ listKeys(value, prefixes :+ key)
    }

    /* flattens the map of done and remaining items into leaves */
    def addLeaves(
      tupledMap: (JsMap, JsMap)): Seq[String] = {
      val (done, remaining) = tupledMap
      done.keySet.map(buildKey).toSeq ++ remaining.foldLeft(Seq[String]())(appendNodes)
    }

    safeCastToMap(conf)
      .map(_.partition(isLeaf)) // a 'leaf' is the actual thing, vs a nested structure
      .map(addLeaves)
      .getOrElse(Seq())
  }

  /* first problem: given a configuration, return a value for a key if it exists */
  println("first problem \n\n")
  val results = Seq(
    get(conf, "serviceA.host"), // -> "localhost"
    get(conf, "serviceB.db.dbname"), // -> "mydb"
    get(conf, "serviceC"), // -> { ... all of serviceC's config }
    get(conf, "serviceA.hosthost")) // -> raise an error

  results.foreach(println)

  /* second problem: list all the keys in the configuration */
  println("second problem \n\n")
  listKeys(conf).foreach(println)
}
