



import scala.io.Source
import scala.util.{Failure, Random, Success, Try}
import scala.xml.XML

object Main {
    def main(args:Array[String]){

        val chemin_corpus:String = "corpus.txt"
        val chemin_dictionnaire:String = "dicorimes.dmp"

      /*
      Gestion des erreurs d'ouverture de fichier par un Try
       */
        Phrases.extraire_phrases(chemin_corpus,chemin_dictionnaire) match {

          case Success(texte) =>val poeme = new DeuxVers(texte)
              /*
              Gestion des cas ou il est impossible d'écrire un poème
               */
              poeme.ecrire() match {
                case Some(t)=>println(t)
                case None=>println("Le corpus ne permet pas l'criture d'un poème")
              }

          case Failure (e)=> println("Problème lors de l'ouverture des fichiers")
        }


    }
}


abstract class Poeme(phrases:List[Phrase]){
    /*Renvoie des phrases aléatoirement*/
    def choose():List[Phrase] = {
        for {i<-List.range(0,phrases.length)}
            yield phrases((new Random).nextInt.abs % phrases.length)
    }

    /*Renvoie au hasard des couples de phrases qui riment*/
  /*
  Remplacer for comprehension by map, flatmap and filter
   */
    def choose_deux():List[(Phrase,Phrase)] = {
    Random.shuffle(
      phrases.flatMap(p1=> phrases filter( p2=> !p1.toString().equals(p2.toString()) &&  (p1 rime_avec p2)) map (p2=>(p1,p2)) )
    )
  }

    /*Renvoie un poème*/
    def ecrire():Option[String]
}
class DeuxVers(phrases:List[Phrase]) extends Poeme(phrases:List[Phrase]){
   /*
    * Écrire un petit poème de deux vers seulement
    * Utilisez choose_deux()
    * Faites en sorte que la différence du nombre de syllabes entre les deux vers
    * ne soit pas trop grande.
    */
    /*
    Utilisation de Tuple
    fonction de haut niveau (filter)
    fonction annonime (p=>(p._1.syllabes<=(p._2.syllabes+2))&&(p._1.syllabes>=(p._2.syllabes-2))
     */
   def memeTaille(p:(Phrase,Phrase)):Boolean = (p._1.syllabes<=(p._2.syllabes+2))&&(p._1.syllabes>=(p._2.syllabes-2))

   def toStringPoeme(p:(Phrase,Phrase))= p._1.toString+"\n"+p._2.toString
   def ecrire():Option[String] = {
     val po = this.choose_deux().filter(p=>(p._1.syllabes<=(p._2.syllabes+2))&&(p._1.syllabes>=(p._2.syllabes-2)))

     if(po.nonEmpty) Some(toStringPoeme(po.last))
     else None
    }


}
/*
Case class, abstract class et héritage
 */

abstract class Phone
case class Voyelle(char: Char)extends Phone
case class Consonne(char:Char)extends Phone

class Mot(val mot:String,val syl:Int,val pho:String) {

  val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")



  def getPhone():Phone=makePhone(this.pho.last)

    override def toString():String = mot +" nb syl = "+syl+" phonetique : "+pho
    /*
     * Deux mots m1 et m2 riment ssi:
     *   le dernier phone (son) de m1 et le dernier phone de m2 sont des voyelles identiques
     *   OU
     *   le dernier phone de m1 et le dernier phone de m2 sont des consonnes identiques ET les deux mots, amputés de ces deux consonnent, riment
     * Cela se prête bien à du pattern matching sur les phones.  Peut-être que deux cases class Voyelle et Consonne, qui "étendent" une classe Phone seraient judicieuses à utiliser...
     * Pour celle-ci, vous avez le droit de considérer que les voyelles correspondent aux écritures phonétiques suivantes:
     * val voyelles = Set("a","e","i","o","u","y","à","è","ù","é","â","ê","î","ô","û","ä","ë","ï","ö","ü","E","§","2","5","9","8","£","@")
     */
  def makePhone(phone:Char):Phone=if (voyelles.contains(phone.toString)){Voyelle(phone)} else {Consonne(phone)}

  def rime_consonne(p1:String,p2:String):Boolean={
    if((p1.length==1) ||(p2.length==1)) false
    else {
      val pho1=p1.takeRight(2)
      val pho2=p2.takeRight(2)

      if(p1.last.equals(p2.last)) {
        (makePhone(pho1.head),makePhone(pho2.head)) match {
          case (Voyelle(v1),Voyelle(v2))=> v1.equals(v2)
          case _=>false
        }

      }
      else false
    }
  }
  /**
    * Tuple
    * Pattern matching
    */


    def rime_avec(autre_mot:Mot):Boolean = (this.getPhone(),autre_mot.getPhone()) match{
    case (Voyelle(v1),Voyelle(v2))=> v1.equals(v2)
    case (Consonne(c1),Consonne(c2))=>c1.equals(c2) && rime_consonne(autre_mot.pho,this.pho)
    case _ =>false
    }

}


class Phrase(phrase:String,mots_hachage:Map[String,Mot]){
    /*
     * Un token est un groupe de lettre séparé par des signes de ponctuation
     * (notamment des espaces).  C'est ce qu'on appelle généralement des "mots".
     */
    private val tokens = Phrases.split_mots(phrase.toLowerCase)
    
    /*La liste des mots de la phrase*/    
    val mots = for {
                    t<-tokens
                } yield mots_hachage(t)

    override def toString():String = phrase

  /*
  Utilisation Option
   */
    def lastMot():Option[Mot]={mots.lastOption}
    
    /*
     * Déterminez le nombre de syllabes de la phrase.
     * Pour bien faire, utilisez map sur la liste de mots, remplacez
     * chaque mot par son nombre de syllabes et utilisez .sum sur la liste
     * qui en résulte
     */
  /**
    * fonctions de haut niveau(map)
    * fonctions anonymes (x=>x.syl)
    */
    val syllabes:Int =   mots.map(x=>x.syl).sum

    /*Deux phrases riment si le dernier mot de l'une rime avec le dernier mot de l'autre.*/

  /*
  pattern matching Option
  lastMot renvoie None si pas de mots
   */
    def rime_avec(phrs:Phrase):Boolean = (lastMot(),phrs.lastMot())match {
      case (Some(m1),Some(m2)) => m1.rime_avec(m2)
      case _=>false
  }
}

/*Cet object compagnon permet de créer une phrase sans utiliser new Phrase(...) mais en mettant directement Phrase(...)*/
object Phrase{
    def apply(phrase:String,mots_hachage:Map[String,Mot]) = new Phrase(phrase,mots_hachage)
}


object Phrases{
    def split_mots(s:String):Array[String] =  s.trim.toLowerCase.split("[- ’—,;'()\"!.:?]+")
    def split_phrases(s:String):Array[String] = s.split("(?<=[.!?:])")
    def lire_csv(chemin:String,mots:Set[String]):Try[List[String]] ={ Try{(for {line <- Source.fromFile(chemin).getLines()  if mots contains line.split(",")(1)} yield line).toList }}

    def doMot(m: Array[String]):(String,Mot)=(m(1),new Mot(m(1),m(6).toInt,m(8)))
    def extraire_phrases (chemin_txt :String , chemin_dico : String ):Try [List[Phrase]] = {
     for {
       texte <-Try(Source.fromFile(chemin_txt).getLines().filter(_!="").foldLeft(""){_+_})
       phrases_txt = split_phrases(texte).toList
       mots_set:Set[String] = (for{mot<-split_mots(texte)}yield mot).toSet

       dico <- lire_csv(chemin_dico,mots_set)


       mots_hachage:Map[String,Mot] = (for(m<-for(d<-dico)yield d.split(",")
       )yield m).map(doMot(_)).toMap

      /*
      Utilisation map and filter
       */
       p= phrases_txt.filter(p=>((split_mots(p) map (mots_hachage contains _)) forall (x=>x)) && p.trim!="").map(t=>Phrase(t.trim,mots_hachage))

      } yield p
  }

}

