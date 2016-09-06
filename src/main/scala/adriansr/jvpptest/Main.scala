package adriansr.jvpptest

import java.util.concurrent.CompletionStage
import java.util.function.BiConsumer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.{Failure, Success, Try}

import org.openvpp.jvpp.callback.JVppCallback
import org.openvpp.jvpp.core._
import org.openvpp.jvpp.core.callback.{AfPacketCreateCallback, AfPacketDeleteCallback}
import org.openvpp.jvpp.core.dto._
import org.openvpp.jvpp.core.future.FutureJVppCoreFacade
import org.openvpp.jvpp.{VppCallbackException, _}

/**
  * Created by adrian on 06/09/16.
  */
object Main {

    val ConnectionName = "test"
    val HostIfName = "enp0s3"
    //val MacAddr = "62:44:4e:de:89:c1"

    object TestCallback extends JVppCallback
                           with AfPacketCreateCallback
                           with AfPacketDeleteCallback {

        override def onError(e: VppCallbackException): Unit = {
            println(s"VPP error[${e.getCtxId}]: $e")
        }

        override def onAfPacketCreateReply(afPacketCreateReply: AfPacketCreateReply): Unit = {
            println(s"AfPacketCreate completed ${afPacketCreateReply.context}")
        }

        override def onAfPacketDeleteReply(afPacketDeleteReply: AfPacketDeleteReply): Unit = {
            println(s"AfPacketDelete completed ${afPacketDeleteReply.context}")
        }
    }

    def timeOp[T](name: String, op: => T): Try[T] = {
        val startTime = System.nanoTime()
        val retval = Try { op }
        val took = (System.nanoTime - startTime) / 1000000
        if (retval.isSuccess) {
            println(s"Operation $name completed in $took ms with value ${retval.get}")
        } else {
            println(s"Operation $name failed in $took ms with exception ${retval.failed.get}")
        }
        retval
    }


    def toScalaFuture[T](cs: CompletionStage[T])(implicit ec: ExecutionContext): Future[T] = {
        val promise = Promise[T]
        cs.whenComplete(new BiConsumer[T, Throwable] {
            def accept(result: T, err: Throwable): Unit = {
                if (err eq null) {
                    promise.success(result)
                } else {
                    promise.failure(err)
                }
            }
        })
        promise.future
    }

    def main(args: Array[String]): Unit = {

        val registry = timeOp[JVppRegistry]("registration",
                                            new JVppRegistryImpl(ConnectionName))
        require (registry.isSuccess)

        val internalLib = new JVppCoreImpl
        val lib = new FutureJVppCoreFacade(registry.get, internalLib)

        //registry.get.register(lib, TestCallback)

        // equivalent to:
        // vpp# delete host-interface name $HostIfName
        val delMsg = new AfPacketDelete
        delMsg.hostIfName = HostIfName.toCharArray.map( _.toByte )
        //val delFuture = toScalaFuture(timeOp("send afPacketDelete", lib.afPacketDelete(delMsg)).get)

        //delFuture onComplete {
        //    case Success(id) => println(s"afPacketDelete completed $id")
        //    case Failure(err) => println(s"afPacketDelete failed $err")
        //}

        // equivalent to:
        // vpp# create host-interface name $HostIfName
        val createMsg = new AfPacketCreate
        createMsg.hostIfName = HostIfName.toCharArray.map( _.toByte )
        //msg.hwAddr = MacAddr.toCharArray.map( _.toByte )
        createMsg.useRandomHwAddr = 1

        //val r = timeOp("send afPacketCreate", lib.afPacketCreate(createMsg))
        //require(r.isSuccess)

        //toScalaFuture(r.get) onComplete {
        //    case Success(id) => println(s"AfPacketCreate completed $id")
        //    case Failure(err) => println(s"AfPacketCreate failed $err")
        //}

        // equivalent to:
        // vpp# set int state host-$HostIfName up
        val setUpMsg = new SwInterfaceSetFlags
        setUpMsg.adminUpDown = 1
        setUpMsg.deleted = 0
        setUpMsg.linkUpDown = 1
        setUpMsg.swIfIndex = -1 // filled later

        for {
            _ <- toScalaFuture(
                timeOp("send afPacketDelete", lib.afPacketDelete(delMsg)).get)
                .collect({ case _ => false })
            creation <- toScalaFuture(
                timeOp("send afPacketCreate", lib.afPacketCreate(createMsg))
                    .get)
            setup <- toScalaFuture(
                timeOp("send set int state", lib.swInterfaceSetFlags(setUpMsg))
                    .get)
        } yield true

        //val delFuture = toScalaFuture[Unit](timeOp("send afPacketDelete", lib.afPacketDelete(delMsg)).get)
        //delFuture.recover {
        //    case _ =>
        //} flatMap {
            //toScalaFuture(timeOp("send afPacketCreate", lib.afPacketCreate(createMsg)).get)
        //}
        //delFuture.collect { case _ =>
        //    toScalaFuture(timeOp("send afPacketCreate", lib.afPacketCreate(createMsg)).get)
        //} flatMap { reply =>
        //    setUpMsg.swIfIndex = reply.
        //    toScalaFuture(timeOp("send set int state", lib.swInterfaceSetFlags(setUpMsg)).get)
        //} onComplete {
        //    case Success(_) => println("Completed!")
        //    case Failure(err) => println(s"Failed: $err")
        //}
    }

    def main_with_basic_jvpp(args: Array[String]): Unit = {

        val registry = timeOp[JVppRegistry]("registration",
                                          new JVppRegistryImpl(ConnectionName))
        require (registry.isSuccess)

        val lib = new JVppCoreImpl
        registry.get.register(lib, TestCallback)

        // equivalent to:
        // vpp# delete host-interface name $HostIfName
        val delMsg = new AfPacketDelete
        delMsg.hostIfName = HostIfName.toCharArray.map( _.toByte )
        timeOp("send afPacketDelete", lib.afPacketDelete(delMsg))

        // equivalent to:
        // vpp# create host-interface name $HostIfName
        val createMsg = new AfPacketCreate
        createMsg.hostIfName = HostIfName.toCharArray.map( _.toByte )
        //msg.hwAddr = MacAddr.toCharArray.map( _.toByte )
        createMsg.useRandomHwAddr = 1
        val r = timeOp("send afPacketCreate", lib.afPacketCreate(createMsg))
        require(r.isSuccess)
    }
}
