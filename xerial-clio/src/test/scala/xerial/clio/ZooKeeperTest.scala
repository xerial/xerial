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
// ZooKeeperTest.scala
// Since: 2012/10/24 10:17 AM
//
//--------------------------------------

package xerial.clio

import xerial.core.XerialSpec
import collection.JavaConversions._
import com.netflix.curator.test.{TestingCluster, TestingServer}
import com.netflix.curator.framework.{CuratorFrameworkFactory, CuratorFramework}
import com.netflix.curator.retry.ExponentialBackoffRetry
import com.google.common.io.Closeables
import org.scalatest.BeforeAndAfter
import org.apache.log4j.{ConsoleAppender, BasicConfigurator}
import java.util.concurrent.{TimeUnit, Callable}
import com.netflix.curator.framework.recipes.leader.{LeaderSelectorListener, LeaderSelector}
import java.io.Closeable
import com.netflix.curator.framework.state.ConnectionState
import util.Random
import com.netflix.curator.utils.{ZKPaths, EnsurePath}


/**
 * @author leo
 */
class ZooKeeperTest extends XerialSpec with BeforeAndAfter {

  var server: TestingServer = null
  var client: CuratorFramework = null

  //BasicConfigurator.configure
  BasicConfigurator.configure(new ConsoleAppender())

  before {
    debug("starting zookeeper server")
    server = new TestingServer
    info("zookeeper server string: %s", server.getConnectString)
    client = CuratorFrameworkFactory.newClient(server.getConnectString, new ExponentialBackoffRetry(1000, 3))
    client.start
  }

  after {
    Closeables.closeQuietly(client)
    Closeables.closeQuietly(server)
  }

  class LeaderSelectorExample(val client:CuratorFramework, path:String, name:String) extends Closeable with LeaderSelectorListener {

    private var ourThread : Thread = null

    private val leaderSelector = new LeaderSelector(client, path, this)
    leaderSelector.autoRequeue

    def start {
      info("starting %s", name)
      leaderSelector.start
    }

    def stateChanged(client: CuratorFramework, newState: ConnectionState) {
      if(newState == ConnectionState.LOST || newState == ConnectionState.SUSPENDED) {
        if(ourThread != null)
          ourThread.interrupt
      }
    }

    def takeLeadership(client: CuratorFramework) {
      val path = new EnsurePath("/xerial-clio/leader")
      path.ensure(client.getZookeeperClient)

      val prevLeader = new String(client.getData().forPath("/xerial-clio/leader"))

      info("%s takes the leadership (previous leader was %s)", name, prevLeader)

      debug("leader selector has %d participants", leaderSelector.getParticipants.size())
      ourThread = Thread.currentThread
      try {
        client.setData().forPath("/xerial-clio/leader", name.getBytes)

        Thread.sleep(TimeUnit.SECONDS.toMillis(1))
      }
      catch {
        case e:InterruptedException => {
          info("%s was interrupted", name)
          Thread.currentThread.interrupt
        }
      }
      finally {
        ourThread = null
        info("%s relinquishing leadership", name)
      }

    }

    def close() {
      info("closing %s", name)
      leaderSelector.close
    }
  }


  "ZooKeeper" should {

    "start a server" in {

      val started = client.isStarted
      started should be (true)

      info("create znode")
      client.create().forPath("/xerial-clio")
      client.create().forPath("/xerial-clio/data")

      info("write data")
      client.setData.forPath("/xerial-clio/data", "Hello ZooKeeper!".getBytes)

      info("read data")
      val s = client.getData().forPath("/xerial-clio/data")
      new String(s) should be("Hello ZooKeeper!")
    }




    "elect a leader" in {
      val clients = for(i <- 0 until 20) yield {
        val c = CuratorFrameworkFactory.newClient(server.getConnectString, new ExponentialBackoffRetry(1000, 3))
        val s = new LeaderSelectorExample(c, "/xerial-clio/test/leader", "client%d".format(i))
        c.start
        s.start
        (c, s)
      }

      Thread.sleep(TimeUnit.SECONDS.toMillis(5))

      for((c, s) <- clients) {
        Closeables.closeQuietly(s)
      }

      for((c, s) <- clients) {
        Closeables.closeQuietly(c)
      }
    }
  }


}


class ZooKeeperEnsembleTest extends XerialSpec with BeforeAndAfter {

  var server: TestingCluster = null

  before {
    server = new TestingCluster(5)
    server.start

    info("started zookeeper ensemble: %s", server.getConnectString)
  }

  after {
    Closeables.closeQuietly(server)
  }

  def withClient[U](f: CuratorFramework => U) : U = {
    val c = CuratorFrameworkFactory.newClient(server.getConnectString, new ExponentialBackoffRetry(1000, 3))
    try {
      c.start
      f(c)
    }
    finally {
      Closeables.closeQuietly(c)
    }
  }


  "ZooKeeperEnsemble" should {

    "run safely if one of the nodes is down" in {
      val m = "Hello Zookeeper Quorum"

      withClient { client =>
        client.create.forPath("/xerial-clio")
        client.create.forPath("/xerial-clio/demo")
        client.setData.forPath("/xerial-clio/demo", m.getBytes)
        val servers = server.getInstances.toSeq
        val victim = servers(Random.nextInt(servers.size))
        info("kill a zookeeper server: %s", victim)
        server.killServer(victim)

        Thread.sleep(TimeUnit.SECONDS.toMillis(1))

        val b = client.getData.forPath("/xerial-clio/demo")
        new String(b) should be (m)

        //info("restart a zookeeper server: %s", victim)
        //server.restartServer(victim)

        //Thread.sleep(TimeUnit.SECONDS.toMillis(1))

        val b2 = client.getData.forPath("/xerial-clio/demo")
        new String(b2) should be (m)
      }

      withClient { client =>
        val b = client.getData.forPath("/xerial-clio/demo")
        new String(b) should be (m)
      }

    }

  }

}