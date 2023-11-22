package controllers

import scala.collection.JavaConverters._
import akka.actor.ActorSystem
import com.google.common.io.BaseEncoding
import dao.{AssemblyReqDAO, ReqSummaryDAO}
import io.circe.Json

import javax.inject._
import models.{Assembly, Summary}
import org.ergoplatform.{ErgoAddressEncoder, ErgoBox}
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{Address, ErgoProver, ErgoValue, Mnemonic, NetworkType, RestApiErgoClient}
import org.ergoplatform.sdk.{ErgoToken, JavaHelpers, SecretString}
import play.api.Logger
import play.api.libs.circe.Circe
import play.api.mvc._
import scalaj.http.Http
import services.NodeService
import sigmastate.Values.ErgoTree
import sigmastate.serialization.ErgoTreeSerializer
import special.collection.Coll
import special.sigma.SigmaProp
import utils.Conf

import scala.concurrent.{ExecutionContext, Future}
import scala.collection.JavaConverters._
import scala.math.BigDecimal.long2bigDecimal




@Singleton
class Controller @Inject()(cc: ControllerComponents, actorSystem: ActorSystem, nodeService: NodeService)
                          (implicit exec: ExecutionContext)
  extends AbstractController(cc) with Circe {

  private val logger: Logger = Logger(this.getClass)

  def errorResponse(e: Exception): Result = {
    val msg = e.getMessage.replaceAll("\n", "\\\\n").replaceAll("\"", "\\\\\"")
    BadRequest(
      s"""{
         |  "success": false,
         |  "detail": "$msg"
         |}""".stripMargin).as("application/json")
  }

  def stop(apiKey: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      if (apiKey == Conf.activeNodeApi) {
        Conf.functioningAdmin = false
        Ok("Stopped functioning")
      } else throw new Exception("Wrong pass")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def start(apiKey: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      if (apiKey == Conf.activeNodeApi) {
        Conf.functioningAdmin = true
        Ok("Started functioning")
      } else throw new Exception("Wrong pass")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def ignoreTime(apiKey: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      if (apiKey == Conf.activeNodeApi) {
        Conf.ignoreTime = true
        Ok("Stopped considering time for requests")
      } else throw new Exception("Wrong pass")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def considerTime(apiKey: String): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      if (apiKey == Conf.activeNodeApi) {
        Conf.ignoreTime = false
        Ok("Time is now being considered for requests")
      } else throw new Exception("Wrong pass")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def willGet(dex: String, box: String, fee: Long): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      nodeService.ergoClient.execute(ctx => {
        var curLp = nodeService.getUnspentBoxFromMempool(dex)
        var buyback = nodeService.getUnspentBoxFromMempool(box)
        if (curLp == null)
          curLp = ctx.getBoxesById(dex).head
        if (buyback == null)
          buyback = ctx.getBoxesById(box).head


        val xRes = curLp.getValue.toBigInt
        val yRes = curLp.getTokens.get(2).getValue
        val lpFee = curLp.getRegisters.get(0).getValue.asInstanceOf[Int].toLong
        val funds = buyback.getTokens.get(0).value
        val willGet = ((xRes * funds * lpFee) / (yRes * 1000L + funds * lpFee)).toLong
        Ok(((willGet - fee)/1e9).toString)
      })

    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def spend(dex: String, box: String, fee: Long): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val mnemonic = "enact blind mobile olive comfort diamond casual sunny tiny glimpse pepper illness damp festival brass"

      nodeService.ergoClient.execute(ctx => {
        var curLp = nodeService.getUnspentBoxFromMempool(dex)
        var buyback = nodeService.getUnspentBoxFromMempool(box)
        if (curLp == null)
          curLp = ctx.getBoxesById(dex).head
        if (buyback == null)
          buyback = ctx.getBoxesById(box).head

        val prover = ctx.newProverBuilder.withMnemonic(SecretString.create(mnemonic), SecretString.create(""), false)
          .withEip3Secret(0)
          .withEip3Secret(1)
          .withEip3Secret(2)
          .build()

        val txB = ctx.newTxBuilder()
        val xRes = curLp.getValue.toBigInt
        val yRes = curLp.getTokens.get(2).getValue
        val lpFee = curLp.getRegisters.get(0).getValue.asInstanceOf[Int].toLong
        val acTokenInd = buyback.getTokens.asScala.indexWhere(_.getId.toString() == Conf.acId)
        val funds = buyback.getTokens.get(acTokenInd).value
        val willGet = ((xRes * funds * lpFee) / (yRes * 1000L + funds * lpFee)).toLong

        var tokensExceptAc: Seq[ErgoToken] = Seq()
        for (i <- 0 until buyback.getTokens.size()) {
          if (i != acTokenInd)
            tokensExceptAc = tokensExceptAc :+ curLp.getTokens.get(i)
        }
        var newBoxx = txB.outBoxBuilder()
          .value(willGet + buyback.getValue - fee)
          .contract(new ErgoTreeContract(buyback.getErgoTree, NetworkType.MAINNET))
        if (tokensExceptAc.nonEmpty)
          newBoxx = newBoxx.tokens(tokensExceptAc: _*)

        val newBox = newBoxx.build()

        val outLp = txB.outBoxBuilder()
          .contract(new ErgoTreeContract(curLp.getErgoTree, NetworkType.MAINNET))
          .value(curLp.getValue - willGet)
          .tokens(curLp.getTokens.get(0), curLp.getTokens.get(1), new ErgoToken(Conf.acId, curLp.getTokens.get(2).getValue + funds))
          .registers(curLp.getRegisters.asScala.toSeq: _*)
          .build()

        val tx = txB.boxesToSpend(Seq(curLp, buyback).asJava)
          .outputs(outLp, newBox)
          .fee(fee)
          .sendChangeTo(Address.fromErgoTree(buyback.getErgoTree, NetworkType.MAINNET))
          .build()
        val signed = prover.sign(tx)
        val txId = ctx.sendTransaction(signed)
        Ok(txId)
      })

    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def state(): Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      Ok(
        s"""{
           |  "functioning": ${Conf.functioning},
           |  "functioningAdmin": ${Conf.functioningAdmin},
           |  "activeNode": "${Conf.activeNodeUrl}",
           |  "ignoreTime": ${Conf.ignoreTime}
           |}""".stripMargin).as("application/json")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }

  def getHeight: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      if (!Conf.functioning || !Conf.functioningAdmin) throw new Exception("Assembler is not functioning currently")
      val height = nodeService.getHeight(Conf.activeNodeUrl)
      Ok(
        s"""{
           |  "height": $height
           |}""".stripMargin).as("application/json")
    } catch {
      case e: Exception => errorResponse(e)
    }
  }

  def info: Action[AnyContent] = Action { implicit request: Request[AnyContent] =>
    try {
      val acBox = nodeService.getAcBox()
      val lpBox = nodeService.getLpBox()
      val r4 = acBox.hcursor.downField("box").downField("additionalRegisters").downField("R4").as[String].getOrElse("")
      val r5 = acBox.hcursor.downField("box").downField("additionalRegisters").downField("R5").as[String].getOrElse("")

      val aucStartInfo = ErgoValue.fromHex(r4).getValue.asInstanceOf[Coll[Long]].toArray
      val auctionLst = ErgoValue.fromHex(r5).getValue.asInstanceOf[Coll[Coll[Long]]].toArray
        .map(_.toArray)
      val auctionLstJson: String = auctionLst.map { case Array(a, b, c, d) =>
        s"""{
           |  "numCoinsToAuction": $a,
           |  "period": $b,
           |  "coef": $c,
           |  "decreaseCoef": $d
           |}""".stripMargin
      }.mkString("[", ",", "]")

      val nextStart = aucStartInfo(0)

      val lpErg = lpBox.hcursor.downField("box").downField("value").as[Long].getOrElse(0L)
      val numAcTok = lpBox.hcursor.downField("box").downField("assets").as[Seq[Json]].getOrElse(Seq())(2)
        .hcursor.downField("amount").as[Long].getOrElse(0L)
      val curPrice: Double = (lpErg.toDouble / numAcTok.toDouble)

      val acLocked = acBox.hcursor.downField("box").downField("assets").as[Seq[Json]].getOrElse(Seq())(1)
        .hcursor.downField("amount").as[Long].getOrElse(0L)

      val currentTime = nodeService.getTimestamp()
      var circulating = Conf.issuanceAmount - acLocked
      if (currentTime < Conf.vestingEnd)
        circulating = circulating - Conf.vesting

      Ok(
        s"""{
           |  "nextStart": ${nextStart},
           |  "curPrice": ${curPrice},
           |  "circulating": ${circulating},
           |  "auctionList": $auctionLstJson
           |}""".stripMargin).as("application/json")
    } catch {
      case e: Exception =>
        errorResponse(e)
    }
  }
}
