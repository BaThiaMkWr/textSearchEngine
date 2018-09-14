package schibsted.assignement.textSearchEngine

import java.io.File

object textSearchEngine extends App {


  try {
    require(args.length > 0, "Usage: Please enter the path to the folder")
  }catch { case _: Throwable => println("Usage: Please enter the path to the folder")
    System.exit(0)
  }

  val topNResults: Int = 10

  val words: String => List[String] = s => s.split("\\W+").map(_.toLowerCase).filterNot(_.trim.isEmpty).toList

  /**
    *
    * @param path
    * @return
    */

  def documentLoader(path : String):  Array[File] = {

    def recursiveListFiles(f: File): Array[File] = {
      val these = f.listFiles
      these ++ these.filter(_.isDirectory).flatMap(recursiveListFiles)
    }
    val files = recursiveListFiles(new File(path)).filter(_.isFile)

    println(s"${files.length} files read in directory $path")

    files
  }

  /**
    *
    * @param docWordVec
    * @param searchWordVec
    * @return
    */

  def searchScore(docWordVec : List[String], searchWordVec : List[String]): Int = {

    val intersection = searchWordVec.intersect(docWordVec)
    val diff = searchWordVec.diff(intersection)

    (intersection.length.toDouble / (intersection.length.toDouble + diff.length.toDouble) *100.0).toInt
  }

  /**
    *
    * @param files
    * @param searchQuery
    * @param topN
    * @return
    */

  def search(files :Array[File], searchQuery: String, topN: Int): Seq[(String, Int)] = {

    try {
      require(topN > 0, s"Top N has to be greater than 0 but was $topN")
    }catch { case _: Throwable => println("Top N has to be greater than 0")
        System.exit(0)
          }


    val scoredDocuments: Seq[(String, Int)] = files.map(file => (file.getName, searchScore(io.Source.fromFile(file).getLines().flatMap(words).toList, words(searchQuery))))
     scoredDocuments
      .sortWith(_._2 > _._2)
      .filterNot(_._2.isNaN)
      .filterNot(_._2 < 1)
      .take(topN)
  }


  val files = documentLoader(args.head)

  println("':quit' to quit")

  while (true) {
    print("search>")
    val input: String = Console.in.readLine()
    if (input == ":quit")
      System.exit(0)

    val topMatches = search(files, input, topNResults)

    if (topMatches.isEmpty)
      println("No matches found")
    else {
      topMatches.foreach( result => println(result._1 + " : " + result._2 +"%"))
    }

  }
}
