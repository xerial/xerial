/*
 * Copyright 2012 Taro L. Saito
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

//--------------------------------------
//
// ClioMain.scala
// Since: 2012/10/24 3:00 PM
//
//--------------------------------------

package xerial.clio

import xerial.cui.{command, option}
import io.Source
import xerial.core.log.Logger
import java.io.File
import com.netflix.curator.framework.CuratorFrameworkFactory
import com.netflix.curator.retry.ExponentialBackoffRetry
import com.netflix.curator.CuratorZookeeperClient


class ZkEnsembleHost(hostName: String, quorumPort: Int = 2888, leaderElectionPort: Int = 3888) {
  def serverName = "%s:%s".format(hostName, quorumPort)
  def name = "%s:%s:%s".format(hostName, quorumPort, leaderElectionPort)
}

object ZkEnsembleHost extends Logger {
  def unapply(s: String): Option[ZkEnsembleHost] = {
    val c = s.split(":")
    try {
      val h = c.length match {
        case 2 => // host:(quorum port)
          new ZkEnsembleHost(c(0), c(1).toInt)
        case 3 => // host:(quorum port):(leader election port)
          new ZkEnsembleHost(c(0), c(1).toInt, c(2).toInt)
        case _ => // hostname only
          new ZkEnsembleHost(s)
      }
      Some(h)
    }
    catch {
      case e => None
    }
  }
}

object ClioMain extends Logger {


  def readHostsFile(fileName:String) : Seq[ZkEnsembleHost] = {
    if (!new File(fileName).exists()) {
      warn("file %s not found", fileName)
      Seq.empty
    }
    else {
      val r = for {
        (l, i) <- Source.fromFile(fileName).getLines().toSeq.zipWithIndex
        h <- l.trim match {
          case ZkEnsembleHost(z) => Some(z)
          case _ =>
            warn("invalid line (%d) in %s: %s", i + 1, fileName, l)
            None
        }
      }
      yield h
      r.toSeq
    }
  }

  def checkZooKeeperServers(servers:Seq[ZkEnsembleHost]) : Boolean = {
    val cs = servers.map(_.serverName).mkString(",")
    // Try to connect the ZooKeeper ensemble using a short delay
    val client = new CuratorZookeeperClient(cs, 600, 150, null, new ExponentialBackoffRetry(10, 2))
    try
    {
      client.start
      client.blockUntilConnectedOrTimedOut()
    }
    catch {
      case e:Exception =>
        warn(e.getMessage)
        false
    }
    finally {
      client.close
    }
  }

//  def startZooKeeperServers(servers:Seq[ZkEnsembleHost]): Boolean = {
//
//
//
//  }

}

/**
 * @author leo
 */
class ClioMain(@option(symbol = "h", name = "help", description = "display help messages")
               val displayHelp: Boolean = false)

  extends Logger {

  import ClioMain._



  @command(description = "Start a ZooKeeper server in this machine")
  def startZooKeeperServer(port: Int) = {
    // read ensemble file $HOME/.clio/zookeeper-ensemble
    val ensembleServers = sys.props.get("user.home") map {
      home =>
        home + "/.clio/zookeeper-ensemble"
    } map readHostsFile

    // Check zoo keeper ensemble instances
    val isRunning = ensembleServers map checkZooKeeperServers getOrElse(false)
    if(!isRunning) {
      // Start zoo keeper servers



    }


  }

}