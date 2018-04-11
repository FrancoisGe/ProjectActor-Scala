import akka.actor.Actor.Receive
import akka.actor._
import akka.routing.{BroadcastPool, RoundRobinPool}

import scala.io.Source
import scala.util.{Failure, Random, Success, Try}
import scala.xml.XML

object GenPoeme extends App {
  val list_chemin_corpus:List[String] = List("dixcontes.txt","daudet.txt","nana.txt")
  val chemin_dictionnaire:String = "dicorimes.dmp"



  beginJob(list_chemin_corpus,chemin_dictionnaire)


  sealed trait PiMessage

  case object Begin extends PiMessage

  case class Work(phrase: Phrase) extends PiMessage
  case class Good(phrase: Phrase) extends PiMessage

  case class StartWork() extends PiMessage
  case class EndWork() extends PiMessage

  case class WorkSens() extends PiMessage
  case class GoodSens(set: Set[String]) extends PiMessage


  class Poete(chemin_corpus: String, chemin_dictionnaire: String) extends Actor {
    var poeme: PhraseRime = _


    override def receive: Receive = {
      case StartWork()=>{

        Phrases.extraire_phrases(chemin_corpus, chemin_dictionnaire) match {

          case Success(texte) => {
            val listPhrase = texte
            poeme = new PhraseRime(texte,context)
            //On envoie la première Phrase au juge et on vérifie qu'elle n'est pas vide

            poeme.firstPhrase() match {

              case Success(p) => sender ! Good(p)
              case Failure(e) => sender ! EndWork

            }
          }


          case Failure(e) => {
            sender ! EndWork
            println("Problème lors de l'ouverture des fichiers : "+ e)

          }
        }

      }
      case Work(phrase) => poeme.foundPhraseOK(phrase) match {
        case Some(p) => {
          sender ! Good(p.last)
          poeme.usePhrase(p.last)
        }
        case None => {
          sender ! EndWork

        }
      }
    }
  }

  class PhraseRime(val phrases: List[Phrase],val poeteContext: ActorContext) {
    // Pour éviter d'avoir toujours la même situation on mélange les phrases
    var phrasesRand = {
      for {i <- List.range(0, phrases.length)} yield phrases((new Random).nextInt.abs % phrases.length)
    }.filter(p=> p.lastMot()match{
      case None => false
      case Some(x)=>true
      })


    def memeTaille(p:(Phrase,Phrase)):Boolean = (p._1.syllabes<=(p._2.syllabes+2))&&(p._1.syllabes>=(p._2.syllabes-2))
    /*
    Renvoie un Option avec la liste des phrases ok
     */
    def foundPhraseOK(pWork: Phrase): Option[List[Phrase]] = {

      val listOk= phrasesRand.filter(p=>  (p rime_avec pWork) && (memeTaille(p,pWork)))
      if (listOk.isEmpty) None
      else Some(listOk)

    }

    def usePhrase(phrase: Phrase): Unit ={
      phrasesRand=phrasesRand.filter(p=>p.equals(phrase))
    }

    /*def sensPhrase(phrase: Phrase):Set[String]={
      val sens_router=poeteContext.actorOf(Props[ActorSens].withRouter(RoundRobinPool(phrase.mots.length)),name="sens_router")
      for (i <- 0 until phrase.mots.length) sens_router ! WorkSens


    }*/

    def firstPhrase(): Try[Phrase] = {
      Try {
        val fp = phrasesRand.last
        phrasesRand = phrasesRand.filter(p => !p.toString().equals(fp.toString()))
        fp
      }
    }

  }


  class Juge(ListNameCorpus: List[String], chemin_dictionnaire: String) extends Actor {
    var nbPoete = ListNameCorpus.length
    var poeteMort=List(self)
    var listPoete=context.children


    override def receive: Receive = {
      case Begin => {
        //On crée les poetes
        ListNameCorpus.map(c => context.actorOf(Props(new Poete(c, chemin_dictionnaire)), name = ("P" + c)))
        context.children.map(a=>a!StartWork())
        listPoete=context.children
      }



      case Good(p) => {
        println(p)
        listPoete.map(a =>  a ! Work(p) )

      }
      case EndWork=>{


        listPoete=listPoete.filter(a=>a!=sender)
        println(" mort : "+ sender)
        println("list : "+listPoete)
        context.stop(sender)

        if(!listPoete.nonEmpty){
          println ("tous les poetes sont mort")
          context.stop(self)
        }


      }
    }
  }

  def beginJob(ListNameCorpus: List[String], chemin_dictionnaire: String): Unit ={
    val system = ActorSystem("MonPoeme")
    val juge = system.actorOf(Props(new Juge(ListNameCorpus,chemin_dictionnaire)),name ="juge")

    juge ! Begin

  }


  class ActorSens(val mot: Mot)extends Actor{

    override def receive: Receive ={
      case WorkSens()=>GoodSens(sens(mot.mot))
    }


    def sens(mot: String): Set[String] ={
      val t=(XML.load("https://fr.wikipedia.org/w/api.php?action=query&titles="
        + mot +
        "&prop=revisions&rvprop=content&format=xml")\\ "rev").toString

      val toRemove = "[{])|<>/*}=»« ".toSet

      val listWord=Phrases.split_mots(t).map(m=>m.filterNot(toRemove)).map(m=>m.replaceAll(" ","")).filter(m=> !m.isEmpty).toSet
      listWord
    }
  }

}


