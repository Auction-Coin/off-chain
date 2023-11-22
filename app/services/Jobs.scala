package services

import akka.actor.{Actor, ActorLogging}
import io.circe.Json
import io.circe.parser.parse
import org.ergoplatform.ErgoAddressEncoder
import org.ergoplatform.appkit.impl.ErgoTreeContract
import org.ergoplatform.appkit.{BlockchainContext, ConstantsBuilder, ErgoValue, InputBox, NetworkType, OutBox}
import org.ergoplatform.sdk.ErgoToken
import play.api.Logger
import scalaj.http.Http
import special.collection.Coll
import utils.Conf
import utils.Conf.HOUR

import scala.collection.immutable.Seq
import scala.collection.JavaConverters._
import scala.math.BigDecimal.long2bigDecimal

object JobsUtil {
  val request = "request"
  val result = "result"
  val handleParams = "params"
  val startAuction = "auction"
  val buyBack = "buyback"
}

class Jobs(nodeService: NodeService) extends Actor with ActorLogging {
  private val logger: Logger = Logger(this.getClass)
  val addressEnc = new ErgoAddressEncoder(NetworkType.MAINNET.networkPrefix)

  def handleParams(): Unit = {
    val maxHeight: Int = Conf.publicNodes.map(url => nodeService.getHeight(url)).max
    val curHeight = nodeService.getHeight(Conf.activeNodeUrl)

    if (maxHeight - curHeight < 3) {
      if (!Conf.functioning) {
        logger.info(s"Node is synced: $curHeight - ${Conf.activeNodeUrl}. Will start functioning")
        Conf.functioning = true
      }
    } else {
      Conf.functioning = false
      logger.error(s"Current active node ${Conf.activeNodeUrl} is not OK, stopped functioning. Will try to switch...")
      Conf.availableNodeUrls.zipWithIndex.foreach(url => {
        val height = nodeService.getHeight(url._1)
        if (maxHeight - height < 3) {
          Conf.activeNodeUrl = url._1
          Conf.activeNodeApi = Conf.availableNodeApis(url._2)
          Conf.activeNodeWallet = Conf.availableNodeWallets(url._2)
          nodeService.defaultHeader = Seq[(String, String)](("Content-Type", "application/json"), ("api_key", Conf.activeNodeApi))
          Conf.functioning = true
          logger.info(s"Found a good node to switch to ${url._1}, started functioning")
        }
      })
    }
  }

  def handleAuctions(): Unit = {
    val acBox = nodeService.getAcBox()
    val r4 = acBox.hcursor.downField("box").downField("additionalRegisters").downField("R4").as[String].getOrElse("")
    val r5 = acBox.hcursor.downField("box").downField("additionalRegisters").downField("R5").as[String].getOrElse("")
    val aucStartInfo = ErgoValue.fromHex(r4).getValue.asInstanceOf[Coll[Long]].toArray
    val auctionInfo = ErgoValue.fromHex(r5).getValue.asInstanceOf[Coll[Coll[Long]]].toArray.map(_.toArray)
    val nextStart = aucStartInfo(0)

    nodeService.ergoClient.execute(ctx => {
      val prover = ctx.newProverBuilder().withDLogSecret(BigInt(0).bigInteger).build()


      val curTime = ctx.createPreHeader().build().getTimestamp
      if (curTime > nextStart) { // we should start action first
        val lpBox = nodeService.getLpBox()
        val lp = ctx.getBoxesById(lpBox.hcursor.downField("box").downField("boxId").as[String].getOrElse("")).head
        val ac = ctx.getBoxesById(acBox.hcursor.downField("box").downField("boxId").as[String].getOrElse("")).head


        val txB = ctx.newTxBuilder()
        val ins = Seq(ac)

        val smAuction = auctionInfo.map(a => a.head).sum
        val outAc = txB.outBoxBuilder()
          .contract(new ErgoTreeContract(ac.getErgoTree, NetworkType.MAINNET))
          .value(ac.getValue - auctionInfo.length * Conf.aucBoxInitVal - Conf.minerFee)
          .tokens(ac.getTokens.get(0), new ErgoToken(Conf.acId, ac.getTokens.get(1).getValue - smAuction))
          .registers(ErgoValue.of(Seq(aucStartInfo(0) + aucStartInfo(1), aucStartInfo(1)).toArray), ac.getRegisters.get(1))
          .build()

        var outs = Seq[OutBox](outAc)
        val stPrice = lp.getValue / lp.getTokens.get(2).getValue
        auctionInfo.foreach(info => {
          val curPrice = stPrice * info.head
          val curStPrice = (curPrice * 1000) / info(2)
          val endPrice = (curPrice * 1000) / info(3)
          val numDec = info(1) / HOUR - 1
          val step = (curStPrice - endPrice) / numDec

          val stTime = curTime + Conf.HOUR / 2
          val auc = txB.outBoxBuilder()
            .contract(new ErgoTreeContract((addressEnc.fromString(Conf.auctionAddr).get).script, NetworkType.MAINNET))
            .value(Conf.aucBoxInitVal)
            .tokens(new ErgoToken(Conf.acId, info.head))
            .registers(ErgoValue.of((addressEnc.fromString(Conf.buyBackAddr).get).script.bytes),
              ErgoValue.pairOf(ErgoValue.of(stTime), ErgoValue.of(stTime + info(1))),
              ErgoValue.of(Seq(curStPrice, step, Conf.HOUR).toArray), ErgoValue.of(Seq[Byte]().toArray))
            .build()
          outs = outs :+ auc
        })

        val tx = txB.boxesToSpend(ins.asJava)
          .addOutputs(outs: _*)
          .fee(Conf.minerFee)
          .sendChangeTo(addressEnc.fromString(Conf.teamAddr).get)
          .addDataInputs(lp)
          .build()
        val signed = prover.sign(tx)
        println(ctx.sendTransaction(signed))
      }
    })
  }

  def handleBuyBack(): Unit = {
    val acBox = nodeService.getAcBox()
    val lpBox = nodeService.getLpBox()
    val minAm: Long  = 10000000
    val buyBackBoxes = nodeService.getBuyBackBoxes().filter(p => {
      val bval = p.hcursor.downField("box").downField("value").as[Long].getOrElse(0L) > minAm
      val hasToken = p.hcursor.downField("box").downField("assets").as[Seq[Json]].getOrElse(Seq()).nonEmpty
      bval || hasToken
    })

    nodeService.ergoClient.execute(ctx => {
      val prover = ctx.newProverBuilder().withDLogSecret(BigInt(0).bigInteger).build()
      var curAc = ctx.getBoxesById(acBox.hcursor.downField("box").downField("boxId").as[String].getOrElse("")).head
      var curLp = ctx.getBoxesById(lpBox.hcursor.downField("box").downField("boxId").as[String].getOrElse("")).head

      val buyBackIns = buyBackBoxes.map(b => nodeService.getUnspentBoxFromMempool(b.hcursor.downField("box").downField("boxId").as[String].getOrElse("")))

      buyBackIns.filter(_.getTokens.size() > 0).foreach(buyback => {
        val txB = ctx.newTxBuilder()
        val ins = Seq(buyback, curAc)

        val outAc = txB.outBoxBuilder()
          .contract(new ErgoTreeContract(curAc.getErgoTree, NetworkType.MAINNET))
          .value(curAc.getValue + (buyback.getValue - Conf.minerFee))
          .tokens(curAc.getTokens.get(0), new ErgoToken(Conf.acId,
            curAc.getTokens.get(1).getValue + buyback.getTokens.get(0).getValue))
          .registers(curAc.getRegisters.asScala.toSeq: _*)
          .build()

        val tx = txB.boxesToSpend(ins.asJava)
          .addOutputs(outAc)
          .fee(Conf.minerFee)
          .sendChangeTo(addressEnc.fromString(Conf.teamAddr).get)
          .addDataInputs(curLp)
          .build()
        val signed = prover.sign(tx)
        println(ctx.sendTransaction(signed))
        curAc = signed.getOutputsToSpend.get(0)

      })
      buyBackIns.filter(_.getTokens.size() == 0).foreach(buyback => {

        val ins = Seq(curLp, curAc, buyback)

        val yRes = curLp.getValue.toBigInt
        val xRes = curLp.getTokens.get(2).getValue
        val lpFee = curLp.getRegisters.get(0).getValue.asInstanceOf[Int].toLong
        val funds = buyback.getValue - Conf.maxMinerFee - (buyback.getValue * Conf.teamFee) / 100
        val willGet = ((xRes * funds * lpFee) / (yRes * 1000L + funds * lpFee)).toLong
        if (willGet == 0) {
          println("not enough funds")
        } else {

          val minerFee = 1e6.toLong
          val newAcVal = 1e7.toLong - minerFee + curAc.getValue
          val newAcTokenVal = curAc.getTokens.get(1).getValue + willGet
          val txB = ctx.newTxBuilder()
          val outAc = txB.outBoxBuilder()
            .contract(new ErgoTreeContract(curAc.getErgoTree, NetworkType.MAINNET))
            .value(newAcVal)
            .tokens(curAc.getTokens.get(0), new ErgoToken(Conf.acId, newAcTokenVal))
            .registers(curAc.getRegisters.asScala.toSeq: _*)
            .build()
          val outLp = txB.outBoxBuilder()
            .contract(new ErgoTreeContract(curLp.getErgoTree, NetworkType.MAINNET))
            .value(curLp.getValue + funds)
            .tokens(curLp.getTokens.get(0), curLp.getTokens.get(1), new ErgoToken(Conf.acId, curLp.getTokens.get(2).getValue - willGet))
            .registers(curLp.getRegisters.asScala.toSeq: _*)
            .build()

          val teamOut = txB.outBoxBuilder()
            .contract(new ErgoTreeContract(addressEnc.fromString(Conf.teamAddr).get.script, NetworkType.MAINNET))
            .value((buyback.getValue * Conf.teamFee) / 100)
            .build()

          val tx = txB.boxesToSpend(ins.asJava)
            .addOutputs(outLp, outAc, teamOut)
            .fee(minerFee)
            .sendChangeTo(addressEnc.fromString(Conf.teamAddr).get)
            .addDataInputs(curLp)
            .build()
          val signed = prover.sign(tx)
          println(ctx.sendTransaction(signed))
          curAc = signed.getOutputsToSpend.get(1)
          curLp = signed.getOutputsToSpend.get(0)
        }
      })
    })
  }

  def receive = {
    case JobsUtil.handleParams =>
      handleParams()
    case JobsUtil.startAuction =>
      try {
//        println("Starting auction")
//        handleAuctions()
      } catch {
        case e: Throwable =>
          logger.error("Error while handling auctions " + e.getMessage)
      }

    case JobsUtil.buyBack =>
      try {
//        println("Starting buyback")
        handleBuyBack()
      } catch {
        case e: Throwable =>
          logger.error("Error while handling buyback " + e.getMessage)
      }
  }


}

