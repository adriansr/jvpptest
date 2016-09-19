package adriansr.jvpptest

import java.util.concurrent.CompletionStage
import java.util.function.BiConsumer

import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{Await, ExecutionContext, Future, Promise}
import scala.concurrent.duration._
import scala.util.control.NonFatal
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

    def vppRequestToFuture[T](name: String,
                              request: => CompletionStage[T]): Future[T] = {
        try {
            toScalaFuture[T](request) andThen {
                case Success(result) => println(s"operation $name completed")
                case Failure(err) => println(s"operation $name failed: $err")
            }
        } catch {
            case NonFatal(err) =>
                Future.failed(err)
        }
    }

    def main(args: Array[String]): Unit = mi_1412()

    class FutureExecutor(delayBetweenTasks: Int = 0,
                         tasks: FutureExecutor.ListType = List()) {

        def add(name: String, op: => Future[Any]): FutureExecutor =
            new FutureExecutor(delayBetweenTasks, (name, () => op) :: tasks)

        def run(): Unit = {
            val rlist = tasks.reverse
            FutureExecutor.runList(delayBetweenTasks,
                                   rlist.head,
                                   rlist.tail) onComplete {
                case Success(_) =>
                    println("Execution succeeded")
                    System.exit(0)
                case Failure(err) =>
                    println(s"Execution failed: $err")
                    System.exit(1)
            }
        }
    }

    object FutureExecutor {
        type FutureCallback = () => Future[Any]
        type ElementType = (String, FutureCallback)
        type ListType = List[ElementType]

        private def runList(delayMs: Int,
                            task: ElementType,
                            pending: ListType): Future[Any] = {
            println(s"Running task ${task._1}")
            task._2().recover {
                case NonFatal(err) =>
                    println(s"Task ${task._1} failed: $err")
                    System.exit(1)
            }.flatMap { _ =>
                delay(delayMs)
            }.flatMap{ _ =>
                    if (pending.nonEmpty) {
                        runList(delayMs, pending.head, pending.tail)
                    } else {
                        val unit = ()
                        Future.successful(unit)
                    }
            }
        }

        private def delay(delayMs: Int): Future[Any] =
            Future{
                if (delayMs > 0) Thread.sleep(delayMs)
            }
    }


    def mi_1412(): Unit = {
        val api = new VppApi("test")

        // returns the created interface index
        def createDevice(name: String, mac: Option[String]): Future[Int] = {
            val macArray = if (mac.isDefined) {
                Some(mac.get.split(":").map(Integer.parseInt(_, 16).toByte))
            } else {
                None
            }

            api.createDevice(name, macArray) flatMap {
                result => api.setDeviceAdminState(result.swIfIndex,
                                                  isUp = true) map
                          (_ => result.swIfIndex)
            }
        }

        val runner = new FutureExecutor(500)

        var ip4rtrdpIndex = -1
        var ip6rtrdpIndex = -1

        runner
            .add("> create host-interface name ip4rtrdp",
                 createDevice("ip4rtrdp", None).map(ip4rtrdpIndex = _))
            .add("> set int ip address ip4rtrdp",
                 api.addDelDeviceAddress(ip4rtrdpIndex,
                                         Array[Byte](10, 0, 0, 1),
                                         24,
                                         isIpv6 = false,
                                         isAdd = true))
            .add("> ip route add",
                 api.addDelRoute(Array[Byte](20, 0, 0, 0),
                                 26,
                                 Some(Array[Byte](10, 0, 0, 2)),
                                 Some(ip4rtrdpIndex),
                                 isAdd = true,
                                 isIpv6 = false))
            .add("> create host-interface name ip6rtrdp",
                 createDevice("ip6rtrdp", None).map(ip6rtrdpIndex = _))
            .add("> set int ip address ip4rtrdp",
                 api.addDelDeviceAddress(ip6rtrdpIndex,
                                         Array[Byte](0x20, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 1),
                                         64,
                                         isIpv6 = true,
                                         isAdd = true))
            .add("> ip route add",
                 api.addDelRoute(Array[Byte](0xbb.toByte, 0xbb.toByte, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0),
                                 48,
                                 Some(Array[Byte](0x20, 1, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 2)),
                                 Some(ip6rtrdpIndex),
                                 isAdd = true,
                                 isIpv6 = true))
            .run()
    }

    /*def mi_1415(): Unit = {
        val api = new VppApi("test")

        // returns the created interface index
        def createDevice(name: String, mac: String): Future[Int] = {
            val macArray = mac.split(":").map(Integer.parseInt(_, 16).toByte)
            api.createDevice(name, Some(macArray)) flatMap {
                result => api.setDeviceAdminState(result.swIfIndex,
                                                  isUp = true) map
                                                        (_ => result.swIfIndex)
            }
        }

        val runner = new FutureExecutor(500)

        var fooId = -1
        var outId = -1

        runner
            .add("> create host-interface foodp",
                   createDevice("foodp", "5e:ab:a6:08:dd:d4").map(fooId = _))
            .add("> create host-interface outdp",
                   createDevice("outdp", "82:b6:10:d0:09:71").map(outId = _))
            .add("> ip route add (net foodp)",
                 api.addDelRoute(Array[Byte](1, 1, 1, 0),
                                 24,
                                 VppApi.NetworkTarget(fooId),
                                 isAdd = true,
                                 isIpv6 = false))
            .add("> ip route add (net foodp)",
                 api.addDelRoute(Array[Byte](1, 1, 1, 2),
                                 32,
                                 VppApi.NetworkTarget(fooId),
                                 isAdd = true,
                                 isIpv6 = false))
            .add("> ip route add (net outdp)",
                 api.addDelRoute(Array[Byte](2, 2, 2, 0),
                                 24,
                                 VppApi.NetworkTarget(outId),
                                 isAdd = true,
                                 isIpv6 = false))
            .add("> ip route add (net outdp)",
                 api.addDelRoute(Array[Byte](2, 2, 2, 1),
                                 32,
                                 VppApi.NetworkTarget(outId),
                                 isAdd = true,
                                 isIpv6 = false))
            .add("> set int ip address foodp",
                   api.addDelDeviceAddress(fooId,
                                           Array[Byte](1, 1, 1, 2),
                                           isIpv6 = false,
                                           isAdd = true))

            .add("> set int ip address outdp",
                   api.addDelDeviceAddress(outId,
                                           Array[Byte](2, 2, 2, 1),
                                           isIpv6 = false,
                                           isAdd = true))
            .add("> ip route add",
                api.addDelRoute(Array[Byte](3, 3, 3, 3),
                                24,
                                VppApi.AddressTarget(Array[Byte](2, 2, 2, 2)),
                                isAdd = true,
                                isIpv6 = false))
            .run()
    }*/

    def main_with_futures(args: Array[String]): Unit = {

        val registry = timeOp[JVppRegistry]("registration",
                                            new JVppRegistryImpl(ConnectionName))
        require(registry.isSuccess)

        val internalLib = new JVppCoreImpl
        val lib = new FutureJVppCoreFacade(registry.get, internalLib)


        // equivalent to:
        // vpp# delete host-interface name <name>
        def deleteDevice(name: String): Future[AfPacketDeleteReply] = {
            val request = new AfPacketDelete
            request.hostIfName = name.toCharArray.map(_.toByte)
            vppRequestToFuture("delete interface",
                               lib.afPacketDelete(request))
        }

        val delMsg = new AfPacketDelete
        delMsg.hostIfName = HostIfName.toCharArray.map(_.toByte)

        // equivalent to:
        // vpp# create host-interface name <name>
        def createDevice(name: String,
                         mac: Option[String]): Future[AfPacketCreateReply] = {

            val request = new AfPacketCreate
            request.hostIfName = name.toCharArray.map(_.toByte)
            mac match {
                case Some(addr) =>
                    request.hwAddr = addr.toCharArray.map(_.toByte)
                    request.useRandomHwAddr = 0
                case None =>
                    request.useRandomHwAddr = 1
            }
            vppRequestToFuture("create interface",
                               lib.afPacketCreate(request))
        }

        // equivalent to:
        // vpp# set int state <interface> up
        def setDeviceUp(ifIndex: Int): Future[SwInterfaceSetFlagsReply] = {
            val setUpMsg = new SwInterfaceSetFlags
            setUpMsg.adminUpDown = 1
            setUpMsg.deleted = 0
            setUpMsg.linkUpDown = 1
            setUpMsg.swIfIndex = ifIndex
            vppRequestToFuture("set up interface",
                               lib.swInterfaceSetFlags(setUpMsg))
        }

        /*for {
            _ <- deleteDevice(HostIfName) recover { case _ => new AfPacketCreateReply }
            creation <- createDevice(HostIfName, None)
            setup <- setDeviceUp(creation.swIfIndex)
        } yield true*/

        deleteDevice(HostIfName).recover {
            case _ => new AfPacketDeleteReply
        } flatMap { _ =>
            createDevice(HostIfName, None)
        } flatMap { result =>
            setDeviceUp(result.swIfIndex)
        }
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
