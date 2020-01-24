import com.sun.tools.doclets.internal.toolkit.util.DocFinder.Input

import scala.io.Source

case class Track(title: String, length: String, rating: Int, features: List[String], writers: List[String])
case class Album(title: String, date: String, artist: String, tracks: List[Track])

val USER_HOME_DIR = System.getProperty("user.home")
val albums =  Source.fromFile(USER_HOME_DIR+"/intelij/src/alben.xml").mkString.toCharArray.toList

def createTokenList(source: List[Char]): List[String] = source match {
  case Nil => Nil
  case '\u0008'::rest =>  createTokenList(rest)
  case '\u0009'::rest =>  createTokenList(rest)
  case '\u000a'::rest =>  createTokenList(rest)
  case '\u000b'::rest =>  createTokenList(rest)
  case '\u000c'::rest =>  createTokenList(rest)
  case '\u000d'::rest =>  createTokenList(rest)
  case '\u0020'::rest =>  createTokenList(rest)
  case '\u0085'::rest =>  createTokenList(rest)
  case '\u200E'::rest =>  createTokenList(rest)
  case '\u200F'::rest =>  createTokenList(rest)
  case '\u2028'::rest =>  createTokenList(rest)
  case '\u2029'::rest =>  createTokenList(rest)
  case '\u3000'::rest =>  createTokenList(rest)
  case '<'::rest => createTokenListInKlammer(rest,"") ::  createTokenList(createTokenListFindeClosingKlammer(rest))
  case _::rest => createTokenListAuserhalbKlammer(source,"") ::  createTokenList(createTokenListFindeOpeningKlammer(rest))


}

def createTokenListInKlammer(source: List[Char], meinString: String): String = source match {

  case '>' ::  rest => meinString
  case x::rest => createTokenListInKlammer(rest, meinString+x)
  case Nil => ""
}

def createTokenListAuserhalbKlammer(source: List[Char], meinString: String):String = source match {

  case '<'::rest => meinString
  case x::rest => createTokenListAuserhalbKlammer(rest, meinString+x)
  case Nil => ""
}


def createTokenListFindeClosingKlammer(source: List[Char]): List[Char] = source match {
  case '>'::rest => rest
  case x::rest => createTokenListFindeClosingKlammer(rest)
  case Nil => source
}

def createTokenListFindeOpeningKlammer(source: List[Char]): List[Char] = source match {
  case '<'::rest => source
  case x::rest => createTokenListFindeOpeningKlammer(rest)
  case Nil => source
}

def parseFile(source:List[String]): List[Album] = source match {
  case "album"::rest => parseAlbum(rest, Album("","","",Nil))::parseFile(findEndofAlbum(rest))
  case _::rest => parseFile(rest)
  case Nil => Nil
}

def parseAlbum(source: List[String], meinAlbum: Album):Album = source match {
  case "track"::rest => parseAlbum(findEndofTrack(rest),meinAlbum.copy(tracks=parseTrackList(rest,meinAlbum.tracks, Track("","",0,Nil,Nil))))
  case "title"::rest => parseAlbum(findEndofTitle(rest), meinAlbum.copy(title=parseTitel(rest)))
  case "artist"::rest => parseAlbum(findEndofArtist(rest), meinAlbum.copy(artist=parseArtist(rest)))
  case "date"::rest => parseAlbum(findEndofDate(rest), meinAlbum.copy(date=parseDate(rest)))
  case "/album"::rest => meinAlbum
  case _::rest => parseAlbum(rest, meinAlbum)
  case Nil => meinAlbum
}



/*Track List*/

def parseTrackList(source: List[String], meineTrackList: List[Track], meinTrack: Track): List[Track] = source match {
  case "feature"::rest => parseTrackList(findEndofFeature(rest), meineTrackList, meinTrack.copy(features = parseFeatureList(rest, meinTrack.features)))
  case "writing"::rest => parseTrackList(findEndofWriting(rest), meineTrackList, meinTrack.copy(writers = parseWritingList(rest, meinTrack.writers)))
  case "title"::rest => parseTrackList(findEndofTitle(rest), meineTrackList, meinTrack.copy(title = parseTitel(rest)))
  case "length"::rest => parseTrackList(findEndofLength(rest), meineTrackList, meinTrack.copy(length = parseLength(rest)))
  case "rating"::rest => parseTrackList(findEndofRating(rest), meineTrackList, meinTrack.copy(rating = parseRating(rest)))
  case "/track"::rest => meineTrackList :+ meinTrack
  case _::rest => parseTrackList(rest,meineTrackList,meinTrack)
  case Nil => Nil
}


/* parse Helper Funktionen*/

def parseFeatureList(source: List[String], myFeatureList: List[String]):List[String] = source match {
  case "/feature"::rest => myFeatureList
  case x::rest => myFeatureList :+ x
  case Nil => myFeatureList
}

def parseWritingList(source: List[String], myFeatureList: List[String]):List[String] = source match {
  case "/writing"::rest => myFeatureList
  case x::rest => myFeatureList :+ x
  case Nil => myFeatureList
}

def parseTitel(source: List[String]): String = source match{
  case "/title"::rest => ""
  case x::rest => x
  case Nil => ""
}

def parseWriting(source: List[String] ): String = source match{
  case "/writing"::rest => ""
  case x::rest => x
  case Nil => ""
}

def parseRating(source: List[String] ): Int = source match{
  case "/rating"::rest => 0
  case "1"::rest => 1
  case "2"::rest => 2
  case "3"::rest => 3
  case "4"::rest => 4
  case "5"::rest => 5
  case _::rest => 0
  case Nil => 0
}

def parseLength(source: List[String] ): String = source match{
  case "/length"::rest => ""
  case x::rest => x
  case Nil => ""
}

def parseArtist(source: List[String]): String = source match{
  case "/artist"::rest => ""
  case x::rest => x
  case Nil => ""
}

def parseDate(source: List[String]): String = source match{
  case "/date"::rest => ""
  case x::rest => x
  case Nil => ""
}

/*Helper Funktionen, um Closing Tags zu finden*/
def findEndofAlbum(source: List[String]): List[String] = source match{
  case "/album"::rest => rest
  case _::rest => findEndofAlbum(rest)
  case Nil => Nil
}

def findEndofTitle(source: List[String]): List[String] = source match{
  case "/title"::rest => rest
  case _::rest => findEndofTitle(rest)
  case Nil => Nil
}

def findEndofTrack(source: List[String]): List[String] = source match{
  case "/track"::rest => rest
  case _::rest => findEndofTrack(rest)
  case Nil => Nil
}

def findEndofDate(source: List[String]): List[String] = source match{
  case "/date"::rest => rest
  case _::rest => findEndofDate(rest)
  case Nil => Nil
}

def findEndofArtist(source: List[String]): List[String] = source match{
  case "/artist"::rest => rest
  case _::rest => findEndofArtist(rest)
  case Nil => Nil
}

def findEndofWriting(source: List[String]): List[String] = source match{
  case "/writing"::rest => rest
  case _::rest => findEndofWriting(rest)
  case Nil => Nil
}

def findEndofFeature(source: List[String]): List[String] = source match{
  case "/feature"::rest => rest
  case _::rest => findEndofFeature(rest)
  case Nil => Nil
}

def findEndofLength(source: List[String]): List[String] = source match{
  case "/length"::rest => rest
  case _::rest => findEndofLength(rest)
  case Nil => Nil
}

def findEndofRating(source: List[String]): List[String] = source match{
  case "/rating"::rest => rest
  case _::rest => findEndofRating(rest)
  case Nil => Nil
}

val Resultat = createTokenList(albums)
val Resultat2 = parseFile(Resultat)



/***************************************************/
/******* PRAKTIKUM 3 *******************************/
/***************************************************/


val MichaelJackson = Resultat2(1)


/** Aufgabe 1 **/
def map[A](func: A=>A, input_list:List[A]):List[A] = input_list match {
  case Nil => Nil
  case x::xs => func(x)::map(func,xs)
}

map[String](x => x.toUpperCase(), "a"::"b"::"c"::"d"::Nil)

val Resultat1b = map[Album](x => x.copy(title = x.title.toUpperCase), Resultat2)
val Resultat1c = map[Album](x => {val aktuellesAlbum = x.copy(title = x.title.toUpperCase); aktuellesAlbum.copy(tracks = map[Track](x => x.copy(title = x.title.toUpperCase), x.tracks))}, Resultat2 )


def poly_map[A,B](func: A=>B, input_list:List[A]):List[B] = input_list match {
  case Nil => Nil
  case x::xs => func(x)::poly_map(func,xs)
}

val Resultat1e = poly_map[Album, List[String]](x => poly_map[Track, String](x => x.length, x.tracks), Resultat2)

def filter [A](condition: A=>Boolean, input_list: List[A]):List[A] = input_list match{
  case Nil => Nil
  case x::xs => if(condition(x)) x::filter(condition,xs) else filter(condition,xs)
}


/** Aufgabe 2 **/
val erg = filter[Int](x => x % 2==0, 1::2::3::4::4::5::5::6::7::Nil)

val Resultat2a = filter[Track](x => x.rating >= 4, MichaelJackson.tracks)
val Resultat2b = poly_map[Track, String](x => x.title, filter[Track](x => x.writers.contains("Rod Temperton"), MichaelJackson.tracks ))



/** Aufgabe 3 **/
def partition[A](condition: A => Boolean, input_list: List[A]): List[List[A]] =  input_list match {
      case Nil => List(List())
      case x::xs => condition(x) match {
        case true => List()::partition(condition, xs)
        case false =>  val y = partition(condition, xs); (x::y.head)::y.tail
      }
}


val Resultat8 = partition[String](x => x == x.toUpperCase , "a"::"b"::"c"::"D"::"e"::"f"::"G"::"H"::"i"::"J"::Nil)
val Resultat3b = partition[String](x => x == "Thriller", poly_map[Track, String](x => x.title, MichaelJackson.tracks))

/** Resultat 3c **/
val Resultat3c1 = partition[Char](x => (x == '\u0008') || (x == '\u0009') || (x == '\u000a') || (x == '\u000b') || (x == '\u000c') || (x == '\u000d') ||  (x == '\u0085') || (x == '\u200E') || (x == '\u200F') || (x == '\u2028') || (x == '\u2029') || (x == '\u3000')|| (x == '<') || (x == '>') , albums)
val Resultat3c2 = poly_map[List[Char],String](x => x.mkString, Resultat3c1 )
val Resultat3c3 = filter[String](x => !(x.isEmpty), Resultat3c2)

/** Aufgabe 4 **/

def higherHigherOrder(function: Int => Int, sumProd:(Int, Int)=>Int, neutral: Int, a:Int, b:Int):Int =
    if (a>b) neutral else sumProd(function(a), higherHigherOrder(function, sumProd, neutral, a+1, b))

def fold(operation: (Int,Int)=>Int, neutral: Int, source: List[Int]): Int = source match {
  case Nil => neutral
  case x::xs => operation(x, fold(operation, neutral, xs))
}

def range(a:Int, b:Int): List[Int] = {
  if (a == b) List(b) else a::range(a+1,b)}

val Resultat4a = higherHigherOrder(x=>x, (a,b)=>a*b,1, 1, 5)
val Resultat4b = fold((x,y)=>x*y,1,map[Int](x=>x, range(1,5)))


fold((a,b) => a-b,0 ,1::2::3::4::Nil)