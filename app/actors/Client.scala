package actors

import akka.actor.{Props, ActorRef, Actor}
import org.json4s.{Extraction, DefaultFormats}
import org.json4s.jackson.JsonMethods._
import play.libs.Akka
import scala.util.{Try, Random}
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._

case class Ping(ts: Int)
case class PingMsg(ping: Ping)
case class Hi(id: Int)
case class HiMsg(hi: Hi)
case class Answer(id: Int, result: Int)
case class AnswerMsg(answer: Answer)

case class Message(text: String, emotion: Int, answer: Int)

case class State(var emotion: Int, var loginTS: Long)

object Client {
  def props(ws: ActorRef) = Props(new Client(ws))
  val db: collection.mutable.Map[Int, State] = collection.mutable.Map()
}

class Client(client: ActorRef) extends Actor {

  Akka.system.scheduler.schedule(4 minutes, 8 minutes, self, "8min")
  Akka.system.scheduler.schedule(2 minutes, 3 minutes, self, "3min")

  implicit val formats = DefaultFormats
  var id: Int = -1

  val first = Map("Привет, меня зовут Таня" -> 1)
  val join = Map("Привет! Как дела?" -> 3, "Привет!" -> 1, "Привет. Как ты?" -> 3)
  val min8 = Map("О, смотри! Сегодня по СТС мой любимый 'Университет монстров'! Хочешь посмотреть со мной?" -> 5,"Жду новую серию 'Квеста'" -> 1)
  val min3 = Map("Рианна сыграет главную роль в новом фильме Бессона. Надо же, какой Люк непостоянный" -> 1,
    "Отведите меня кто-нибудь на ВДНХ. Хочу попасть в 'Москвариум'" -> 1,
    "Денис Мацуев сломал рояль за 6 млн рублей. А я тоже умею играть на губной гармошке" -> 1,
    "Вот бы сейчас выпить капучино с карамельным сиропом. Я слышала, что в пекарне 'Мишель' делают лучший кофе в городе" -> 1)
  val tap = Seq("Привет, красавчик!" -> 1,"Как тебе моя новая прическа?" -> 3,"Люблю осень. Чем холоднее на улице, тем приятнее дома" -> 1,
    "Как дела? Что-нибудь важное на сегодня?" -> 4,"Мне нравится твоя улыбка!" -> 3)

  override def receive = {
    case "8min" if sender().equals(self) =>
      client ! pretty(render(Extraction.decompose(
      Message(Random.shuffle(min8).head._1, if (Client.db.get(id).isDefined) Client.db(id).emotion else 0, Random.shuffle(min8).head._2)
    )))
    case "3min" if sender().equals(self) =>
      client ! pretty(render(Extraction.decompose(
        Message(Random.shuffle(min3).head._1, if (Client.db.get(id).isDefined) Client.db(id).emotion else 0, Random.shuffle(min3).head._2)
      )))
    case msg: String => handleMessage(msg, client)
    case _ => println("wtf?")
  }

  def updateState(f: Boolean): Message = {
    if (id > 0) {
      Client.db.synchronized {
        var msg: Message = null
        if (f) {
          if (!Client.db.isDefinedAt(id)) {
            Client.db(id) = State(Random.nextInt(6),System.currentTimeMillis())
            msg = Message(Random.shuffle(first).head._1,Client.db(id).emotion, Random.shuffle(first).head._2)
          } else {
            var text = Random.shuffle(join).head._1
            if (System.currentTimeMillis() - Client.db(id).loginTS > 48*60*60*1000)
            {Client.db(id).emotion = 1; text = "Почему ты меня игнорируешь?"}
            msg = Message(text,Client.db(id).emotion, Random.shuffle(join).head._2)
          }
        } else {
          msg = Message(Random.shuffle(join).head._1,Client.db(id).emotion, Random.shuffle(join).head._2)
        }
        msg
      }
    } else null
  }

  def handleMessage(msg: String, sender: ActorRef) = {
    Try(handleHi(msg, sender))
      .orElse(Try(handlePing(msg, sender))
        .orElse(Try(handleAnswer(msg, sender))
          .orElse(Try(handleElse(msg, sender))
          )))
  }

  def handleHi(msg: String, sender: ActorRef) = {
      val obj = parse(msg).extract[HiMsg]
      id = if (obj.hi.id>0) obj.hi.id else -1
      Client.db.synchronized {
        if (Client.db.isDefinedAt(id))
          Client.db(id).loginTS = System.currentTimeMillis()
      }
      sender ! pretty(render(Extraction.decompose(updateState(true))))
  }

  def handlePing(msg: String, sender: ActorRef) = {
    val obj = parse(msg).extract[PingMsg]
    if (id > 0){
      println(s"ping, ts=${obj.ping.ts}")
      sender ! ("{\"pong\":{\"ts\":time}}".replace("time", System.currentTimeMillis().toString))
    }
  }

  def handleAnswer(msg: String, sender: ActorRef) = {
    val obj = parse(msg).extract[AnswerMsg]
    if (id > 0){
      Client.db.synchronized {
        Client.db(id).emotion = if (Random.nextFloat() > 0.5) 0 else 5
      }
      sender ! pretty(render(Extraction.decompose(
        Message(Random.shuffle(tap).head._1, if (Client.db.get(id).isDefined) Client.db(id).emotion else 0, Random.shuffle(tap).head._2))))
    }
  }

  def handleElse(msg: String, sender: ActorRef) = {
    println(s"wtf?, msg=$msg")
    sender ! "wtf?"
  }

}
