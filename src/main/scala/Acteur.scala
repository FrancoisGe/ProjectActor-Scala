import GenPoeme.PiMessage
import akka.actor.Actor.Receive
import akka.actor._
import akka.routing.RoundRobinPool

import scala.io.Source
import scala.util.{Failure, Random, Success, Try}
import scala.xml._


object ttt{

  def main(args: Array[String]) {
    val mot ="courir"
    (sens(mot)++sens("bonjour")).intersect(sens("courir"))
      .map(m=>println(m))




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



