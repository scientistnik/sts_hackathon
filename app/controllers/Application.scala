package controllers

import play.api.mvc._
import play.api.Play.current

object Application extends Controller {

  def ws = WebSocket.acceptWithActor[String, String] { request => out =>
    actors.Client.props(out)
  }

}