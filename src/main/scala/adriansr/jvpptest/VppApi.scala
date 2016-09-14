package adriansr.jvpptest

import java.util.concurrent.CompletionStage
import java.util.function.BiConsumer

import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.util.control.NonFatal

import org.openvpp.jvpp.JVppRegistryImpl
import org.openvpp.jvpp.core.JVppCoreImpl
import org.openvpp.jvpp.core.dto._
import org.openvpp.jvpp.core.future.FutureJVppCoreFacade

/**
  * Created by adrian on 14/09/16.
  */
class VppApi(connectionName: String)(implicit ec: ExecutionContext) {

    import VppApi._

    private val registry = new JVppRegistryImpl(connectionName)
    private val coreLib = new JVppCoreImpl
    private val lib = new FutureJVppCoreFacade(registry, coreLib)

    // equivalent to:
    // vpp# delete host-interface name <name>
    def deleteDevice(name: String): Future[AfPacketDeleteReply] = {
        val request = new AfPacketDelete
        request.hostIfName = name.toCharArray.map(_.toByte)
        vppRequestToFuture(lib.afPacketDelete(request))
    }

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
        vppRequestToFuture(lib.afPacketCreate(request))
    }

    // equivalent to:
    // vpp# set int state <interface> up
    def setDeviceAdminState(state: Boolean,
                            ifIndex: Int): Future[SwInterfaceSetFlagsReply] = {
        val setUpMsg = new SwInterfaceSetFlags
        setUpMsg.adminUpDown = if (state) 1 else 0
        setUpMsg.deleted = 0
        setUpMsg.linkUpDown = 1
        setUpMsg.swIfIndex = ifIndex
        vppRequestToFuture(lib.swInterfaceSetFlags(setUpMsg))
    }

    //equivalent to:
    // ip route add/del address/prefix via nextHop
    def addDelRoute(address: Array[Byte],
                    prefix: Byte,
                    nextHop: Array[Byte],
                    isAdd: Boolean,
                    isIpv6: Boolean): Future[IpAddDelRouteReply] = {
        val routeMsg = new IpAddDelRoute
        routeMsg.dstAddress = address
        routeMsg.dstAddressLength = prefix
        routeMsg.isAdd = if (isAdd) 1 else 0
        routeMsg.isIpv6 = if (isIpv6) 1 else 0
        routeMsg.nextHopAddress = nextHop
        //routeMsg.resolveIfNeeded = 1
        vppRequestToFuture(lib.ipAddDelRoute(routeMsg))
    }

    //equivalent to:
    // ip route add address/prefix via nextHop
    def addRoute6(address: Array[Byte],
                  prefix: Byte,
                  nextHop: Array[Byte]): Future[IpAddDelRouteReply] = {
        val routeMsg = new IpAddDelRoute
        routeMsg.dstAddress = address
        routeMsg.dstAddressLength = prefix
        routeMsg.isAdd = 1
        routeMsg.isIpv6 = 1
        routeMsg.nextHopAddress = nextHop
        vppRequestToFuture(lib.ipAddDelRoute(routeMsg))
    }
}

object VppApi {
    private def toScalaFuture[T](cs: CompletionStage[T])
                                (implicit ec: ExecutionContext): Future[T] = {
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

    private def vppRequestToFuture[T](request: => CompletionStage[T])
                                     (implicit ec: ExecutionContext): Future[T] = {
        try {
            toScalaFuture[T](request)
        } catch {
            case NonFatal(err) =>
                Future.failed(err)
        }
    }
}
