import scala.collection.mutable.ArrayBuffer
import Console.{YELLOW, RESET}
import scala.sys.process._

/** 文字列中のカタカナをひらがなに変換する */
def katakana2hiragana(str: String): String =
  str.map {
    case char if char >= 'ァ' && char <= 'ヶ' => (char - ('ァ' - 'ぁ')).toChar
    case char => char
  }.mkString("")


/** その駅名の次に使える頭の文字の一覧を返す
  *
  * - 促音 (ー) は無視する
  * - 濁点・半濁点が付く場合はそれらを除いた文字も使用可能 (e.g. "が"終わりの場合"か"からも始められる)
  * - 「ゃゅょ」の場合はそれぞれ「やゆよ」に直す
  * */
def extractNextTurnChars(stationName: String): Seq[String] = {
  val lastChar = stationName.last match {
    case 'ー' => stationName.takeRight(2).head
    case c => c
  }
  (lastChar match {
    case c if "がぎぐげござじずぜぞだじづでどばびぶぺぼ".contains(c) =>
      Seq(c, (c - 1).toChar)
    case c if "ぱぴぷぺぽ".contains(c) =>
      Seq(c, (c - 2).toChar)
    case c if "ゃゅょ".contains(c) =>
      Seq((c + 1).toChar)
    case c => Seq(c)
  }).map(_.toString)
}

/**
  * 駅名一覧にスコアを付与して返す
  *
  * その駅名を選択した時、次に使える駅名の選択肢数をスコアとする
  */
def zipWithScore(stationNames: Seq[String]): Seq[(String, Int)] =
  stationNames.map { name =>
    val nextTurnChars = extractNextTurnChars(name).map(_.toString)
    val score = stationNames.count(name => nextTurnChars.exists(name.startsWith))
    (name, score)
  }

// Wikipediaの日本の鉄道駅一覧の各ページのリストを雑にコピペしてきたファイル
// https://ja.wikipedia.org/wiki/日本の鉄道駅一覧
val src = scala.io.Source.fromFile("wikipedia-ekimei-page-texts.txt")
val lines = src.getLines.to[ArrayBuffer]
src.close()

// 正規表現でひらがなの駅名部分を抽出
val stationNames: ArrayBuffer[String] = lines.flatMap(line => ".+（(.+)えき・?.*）".r.findFirstMatchIn(line).map(m => m.group(1)))
  .map(katakana2hiragana)
  .distinct
  .sorted

println(s"駅名数: ${stationNames.length}") // 7884

var myAnswer = ""
var alexaAnswer = ""

for (turn <- 1 to 10000) {
  println(s"${turn}回目の入力:")
  println(s"Alexaの回答は? (聞き取ってくれないなどで違う候補を出したい場合は n を入力)")

  val input = io.StdIn.readLine()

  input match {
    case "n" => stationNames -= myAnswer // 前の回答は聞き取らせることができないので除去して続ける
    case ans =>
      alexaAnswer = ans
      stationNames -= myAnswer // 前の回答を除外
      stationNames -= alexaAnswer // Alexaの回答を除外
  }

  val thisTurnChars = extractNextTurnChars(alexaAnswer)
  val candidateStationNames = stationNames.filter(name => thisTurnChars.exists(name.startsWith))

  if (candidateStationNames.isEmpty) {
    println("答えられる駅がありません。あなたの負けです")
    sys.exit(-1)
  }

  val namesWithScore: Seq[(String, Int)] = zipWithScore(stationNames)
  val answerAndScore = namesWithScore.filter { case (name, _) => candidateStationNames.contains(name) }.maxBy(_._2)
  myAnswer = answerAndScore._1
  val score = answerAndScore._2
  println(s"つぎは「$YELLOW$myAnswer$RESET」(score: $score)")
  s"say $myAnswer" !!
}
