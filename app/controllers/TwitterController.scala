package controllers

import javax.inject._
import play.api.Configuration
import play.api.libs.oauth._
import play.api.libs.ws.WSClient
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

/**
  * HTTPS configuration explained here: https://www.playframework.com/documentation/2.6.x/ConfiguringHttps
  * JVM parameters placed in IntelliJ run configurations
  *
  * For getting @Before and @After working
  * https://stackoverflow.com/questions/12683176/what-are-play-2-0-equivalents-of-before-and-after-from-play-1-2
  */
@Singleton
class TwitterController @Inject()(implicit ec: ExecutionContext, config: Configuration, cc: ControllerComponents, ws: WSClient) extends AbstractController(cc) {

  def search(term: String) = Action {
    Ok("search term is: "+term)
  }

  def request = Action.async { request: Request[AnyContent] =>
    // make call to endpoint
    val url = "https://www.google.com/"
    ws.url(url).get().map {
      response =>
        Ok(response.body) // raw response
        // Also has: response.json, response.xml
    }
  }

  val CONSUMER_KEY = ConsumerKey(config.get[String]("consumerKey"),config.get[String]("consumerSecret").toString)

  val oauth = OAuth(ServiceInfo(
    "https://api.twitter.com/oauth/request_token",
    "https://api.twitter.com/oauth/access_token",
    "https://api.twitter.com/oauth/authorize", CONSUMER_KEY))

  def sessionTokenPair(implicit request: RequestHeader): Option[RequestToken] = {
    for {
      token <- request.session.get("token")
      secret <- request.session.get("secret")
    } yield {
      RequestToken(token, secret)
    }
  }

  // https://developer.twitter.com/en/docs/twitter-for-websites/log-in-with-twitter/guides/implementing-sign-in-with-twitter.html
  def authenticate = Action { request: Request[AnyContent] =>
    val refererUrl = request.session.get("referrer").getOrElse("/")
    request.getQueryString("oauth_verifier").map { verifier =>
      val tokenPair = sessionTokenPair(request).get
      // We got the verifier; now get the access token, store it and back to index
      oauth.retrieveAccessToken(tokenPair, verifier) match {
        case Right(t) => {
          // We received the authorized tokens in the OAuth object - store it before we proceed
          Redirect(refererUrl).withSession("token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) => throw e
      }
    }.getOrElse(
      //param in retrieveRequestToken must be set as callback URL in Twitter App settings
      oauth.retrieveRequestToken("https://localhost:9443/twitter/auth") match {
        case Right(t) => {
          // We received the unauthorized tokens in the OAuth object - store it before we proceed
          Redirect(oauth.redirectUrl(t.token)).withSession("referrer" -> refererUrl, "token" -> t.token, "secret" -> t.secret)
        }
        case Left(e) =>
          throw e
      })
  }

  def timeline = Action.async { implicit request: Request[AnyContent] =>
    sessionTokenPair match {
      case Some(credentials) => {
        ws.url("https://api.twitter.com/1.1/statuses/home_timeline.json")
          .sign(OAuthCalculator(CONSUMER_KEY, credentials))
          .get
          .map(result => Ok(result.json))
      }
      case _ =>
        Future.successful(Redirect(routes.TwitterController.authenticate)
          .withSession("referrer" -> request.uri))
    }
  }
}
