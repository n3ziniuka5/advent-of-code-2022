package aoc

import aoc.Common.timed

import scala.annotation.tailrec
import scala.collection.mutable
import scala.io.Source

object Day19:
  def main(args: Array[String]): Unit =
    val lines = Source.fromResource("day19.txt").getLines().toList
    timed("Part 1", part1(lines))
    timed("Part 2", part2(lines))

  case class ObsidianRobotCost(ore: Int, clay: Int)
  case class GeodeRobotCost(ore: Int, obsidian: Int)
  case class Blueprint(
    number: Int,
    oreRobotCost: Int,
    clayRobotCost: Int,
    obsidianRobotCost: ObsidianRobotCost,
    geodeRobotCost: GeodeRobotCost
  ):
    val maxObsidianRobots = geodeRobotCost.obsidian
    val maxClayRobots     = obsidianRobotCost.clay
    val maxOreRobots      = List(clayRobotCost, obsidianRobotCost.ore, geodeRobotCost.ore).max

  case class State(
    oreRobots: Int,
    clayRobots: Int,
    obsidianRobots: Int,
    geodeRobots: Int,
    ore: Int,
    clay: Int,
    obsidian: Int,
    geode: Int,
    timeRemaining: Int
  ):
    def addOreRobot(blueprint: Blueprint): State = copy(ore = ore - blueprint.oreRobotCost, oreRobots = oreRobots + 1)
    def addClayRobot(blueprint: Blueprint): State =
      copy(ore = ore - blueprint.clayRobotCost, clayRobots = clayRobots + 1)
    def addObsidianRobot(blueprint: Blueprint): State = copy(
      ore = ore - blueprint.obsidianRobotCost.ore,
      clay = clay - blueprint.obsidianRobotCost.clay,
      obsidianRobots = obsidianRobots + 1
    )
    def addGeodeRobot(blueprint: Blueprint): State = copy(
      ore = ore - blueprint.geodeRobotCost.ore,
      obsidian = obsidian - blueprint.geodeRobotCost.obsidian,
      geodeRobots = geodeRobots + 1
    )

    def canBuildOreRobot(blueprint: Blueprint): Boolean  = ore >= blueprint.oreRobotCost
    def canBuildClayRobot(blueprint: Blueprint): Boolean = ore >= blueprint.clayRobotCost
    def canBuildObsidianRobot(blueprint: Blueprint): Boolean =
      ore >= blueprint.obsidianRobotCost.ore && clay >= blueprint.obsidianRobotCost.clay
    def canBuildGeodeRobot(blueprint: Blueprint): Boolean =
      ore >= blueprint.geodeRobotCost.ore && obsidian >= blueprint.geodeRobotCost.obsidian

    def advanceTimeUntil(pred: State => Boolean, minimumRemainingTime: Int): Option[State] = {
      @tailrec
      def loop(state: State): Option[State] =
        if (state.timeRemaining < minimumRemainingTime) None
        else if (pred(state)) Some(state)
        else loop(state.advanceTime(1))

      loop(this)
    }

    def advanceTime(minutes: Int): State = {
      this.copy(
        timeRemaining = timeRemaining - minutes,
        ore = ore + (oreRobots * minutes),
        clay = clay + (clayRobots * minutes),
        obsidian = obsidian + (obsidianRobots * minutes),
        geode = geode + (geodeRobots * minutes)
      )
    }

    def handsDownBetterThan(other: State): Boolean =
      geode > other.geode && geodeRobots > other.geodeRobots && obsidianRobots > other.obsidianRobots

  def parse(lines: List[String]): List[Blueprint] =
    lines.map {
      case s"Blueprint $nr: Each ore robot costs $oreRobotCost ore. Each clay robot costs $clayRobotCost ore. Each obsidian robot costs $obsidianOreCost ore and $obsidianClayCost clay. Each geode robot costs $geodeOreCost ore and $geodeObsidianCost obsidian." =>
        Blueprint(
          nr.toInt,
          oreRobotCost.toInt,
          clayRobotCost.toInt,
          ObsidianRobotCost(obsidianOreCost.toInt, obsidianClayCost.toInt),
          GeodeRobotCost(geodeOreCost.toInt, geodeObsidianCost.toInt)
        )
    }

  def solve(blueprint: Blueprint, minutes: Int): Int =
    @tailrec
    def loop(
      pq: mutable.PriorityQueue[State],
      bestSeenState: Map[Int, State]
    ): Int =
      val state = pq.dequeue()
      if (state.timeRemaining == 0) state.geode
      else if (bestSeenState(state.timeRemaining).handsDownBetterThan(state)) loop(pq, bestSeenState)
      else
        val buildGeodeRobot =
          if (state.obsidianRobots > 0)
            state
              .advanceTimeUntil(_.canBuildGeodeRobot(blueprint), 2)
              .map(
                _.advanceTime(1)
                  .addGeodeRobot(blueprint)
              )
          else None

        val buildObsidianRobot =
          if (
            !state.canBuildGeodeRobot(blueprint) &&
            state.clayRobots > 0 &&
            state.obsidianRobots < blueprint.maxObsidianRobots
          )
            state
              .advanceTimeUntil(_.canBuildObsidianRobot(blueprint), 3)
              .map(_.advanceTime(1).addObsidianRobot(blueprint))
          else None

        val buildClayRobot =
          if (!state.canBuildGeodeRobot(blueprint) && state.clayRobots < blueprint.maxClayRobots)
            state
              .advanceTimeUntil(_.canBuildClayRobot(blueprint), 3)
              .map(
                _.advanceTime(1)
                  .addClayRobot(blueprint)
              )
          else None

        val buildOreRobot =
          if (!state.canBuildGeodeRobot(blueprint) && state.oreRobots < blueprint.maxOreRobots)
            state.advanceTimeUntil(_.canBuildOreRobot(blueprint), 3).map(_.advanceTime(1).addOreRobot(blueprint))
          else None

        val justWait =
          if (
            buildGeodeRobot.isEmpty &&
            buildObsidianRobot.isEmpty &&
            buildClayRobot.isEmpty &&
            buildOreRobot.isEmpty
          )
            Some(state.advanceTime(state.timeRemaining))
          else None

        val newStates = List(buildGeodeRobot, buildObsidianRobot, buildClayRobot, buildOreRobot, justWait).flatten

        val currentBestSeen = bestSeenState(state.timeRemaining)
        val newBestSeen =
          if (state.handsDownBetterThan(currentBestSeen))
            state
          else currentBestSeen

        pq.enqueue(newStates: _*)
        loop(
          pq,
          bestSeenState + (state.timeRemaining -> newBestSeen)
        )

    val startingState = State(1, 0, 0, 0, 0, 0, 0, 0, minutes)
    loop(
      mutable.PriorityQueue[State](startingState)(Ordering.by(s => (s.timeRemaining, s.geode))),
      Map.empty.withDefaultValue(startingState)
    )

  def part1(lines: List[String]): Int =
    parse(lines).map(b => b.number * solve(b, 24)).sum

  def part2(lines: List[String]): Int =
    parse(lines).take(3).map(solve(_, 32)).product
