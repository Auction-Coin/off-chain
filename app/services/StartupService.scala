package services

import akka.actor.{ActorRef, ActorSystem, Props}
import javax.inject._
import play.api.Logger
import play.api.db.slick.{DatabaseConfigProvider, HasDatabaseConfigProvider}
import play.api.inject.ApplicationLifecycle
import slick.jdbc.JdbcProfile
import utils.Conf

import scala.concurrent.duration._
import scala.concurrent.{ExecutionContext, Future}

@Singleton
class StartupService @Inject()(protected val dbConfigProvider: DatabaseConfigProvider, appLifecycle: ApplicationLifecycle,
                               system: ActorSystem)
                              (implicit ec: ExecutionContext) extends HasDatabaseConfigProvider[JdbcProfile] {

  private val logger: Logger = Logger(this.getClass)

  logger.info("App started!")

  val nodeService = new NodeService()
  val jobs: ActorRef = system.actorOf(Props(new Jobs(nodeService)), "scheduler")

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Conf.auctionStartInterval.seconds,
    receiver = jobs,
    message = JobsUtil.startAuction
  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Conf.buyBackInterval.seconds,
    receiver = jobs,
    message = JobsUtil.buyBack
  )

  system.scheduler.scheduleAtFixedRate(
    initialDelay = 2.seconds,
    interval = Conf.auctionStartInterval.seconds,
    receiver = jobs,
    message = JobsUtil.startAuction
  )

  appLifecycle.addStopHook { () =>
    logger.info("App stopped!")
    Future.successful(())
  }
}
