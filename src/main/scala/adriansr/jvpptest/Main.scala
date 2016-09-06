package adriansr.jvpptest

import scala.util.Try

import org.openvpp.jvpp.core._
//import org.openvpp.jvpp.callback.JVppCallback
import org.openvpp.jvpp.core.dto.AfPacketCreate

/**
  * Created by adrian on 06/09/16.
  */


object Main {

    val ConnectionName = "test"
    val HostIfName = "dev"
    val MacAddr = "62:44:4e:de:89:c1"

    val connectCallback = new JVppCallback {
        override def onError(e: VppCallbackException): Unit = {
            println("Connect callback onError: $e")
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
        //val result = timeOp[JVppRegistry]("registration",
        //                                  new JVppRegistryImpl(ConnectionName))
        //require (result.isSuccess)
        //val lib = result.get

        val lib = new JVppCoreImpl

        val msg = new AfPacketCreate
        msg.hostIfName = HostIfName.toCharArray.map( _.toByte )
        msg.hwAddr = MacAddr.toCharArray.map( _.toByte )
        msg.useRandomHwAddr = 0
        val r = timeOp("afPacketCreate", lib.afPacketCreate(msg))
        require(r.isSuccess)
    }
}
