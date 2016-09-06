package adriansr.jvpptest

import scala.util.Try

import org.openvpp.jvpp._
import org.openvpp.jvpp.core._
import org.openvpp.jvpp.callback.JVppCallback
import org.openvpp.jvpp.core.callback.{AfPacketCreateCallback, AfPacketDeleteCallback}
import org.openvpp.jvpp.VppCallbackException
import org.openvpp.jvpp.core.dto._
import org.openvpp.jvpp.core.future.FutureJVppCoreFacade

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

    def main(args: Array[String]): Unit = {

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
