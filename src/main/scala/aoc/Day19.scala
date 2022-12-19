package aoc

import aoc.Common.timed

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
  )

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
    println(s"solving blueprint - $blueprint with time $minutes")
    def loop(pq: mutable.PriorityQueue[State], visited: Map[Int, Int]): Int =
      val state = pq.dequeue()
      if (state.timeRemaining == 0) state.geode
      else if (visited.getOrElse(state.timeRemaining, 0) > state.geodeRobots) loop(pq, visited)
      else
        // println(state)
        val stateWithAdditionalResources = state.copy(
          timeRemaining = state.timeRemaining - 1,
          ore = state.ore + state.oreRobots,
          clay = state.clay + state.clayRobots,
          obsidian = state.obsidian + state.obsidianRobots,
          geode = state.geode + state.geodeRobots
        )

        val newGeodeRobotState =
          if (state.ore >= blueprint.geodeRobotCost.ore && state.obsidian >= blueprint.geodeRobotCost.obsidian)
            Some(
              stateWithAdditionalResources.copy(
                geodeRobots = state.geodeRobots + 1,
                ore = stateWithAdditionalResources.ore - blueprint.geodeRobotCost.ore,
                obsidian = stateWithAdditionalResources.obsidian - blueprint.geodeRobotCost.obsidian
              )
            )
          else None

        val newObsidianRobotState =
          if (
            newGeodeRobotState.isEmpty && blueprint.maxObsidianRobots > state.obsidianRobots && state.ore >= blueprint.obsidianRobotCost.ore && state.clay >= blueprint.obsidianRobotCost.clay
          )
            Some(
              stateWithAdditionalResources.copy(
                obsidianRobots = state.obsidianRobots + 1,
                ore = stateWithAdditionalResources.ore - blueprint.obsidianRobotCost.ore,
                clay = stateWithAdditionalResources.clay - blueprint.obsidianRobotCost.clay
              )
            )
          else None

        val newClayRobotState =
          if (
            newGeodeRobotState.isEmpty && blueprint.maxClayRobots > state.clayRobots && state.ore >= blueprint.clayRobotCost
          )
            Some(
              stateWithAdditionalResources
                .copy(
                  clayRobots = state.clayRobots + 1,
                  ore = stateWithAdditionalResources.ore - blueprint.clayRobotCost
                )
            )
          else None

        val newOreRobotState =
          if (
            newGeodeRobotState.isEmpty && blueprint.maxOreRobots > state.oreRobots && state.ore >= blueprint.oreRobotCost
          )
            Some(
              stateWithAdditionalResources
                .copy(
                  oreRobots = state.oreRobots + 1,
                  ore = stateWithAdditionalResources.ore - blueprint.oreRobotCost
                )
            )
          else None

        /*val build = newGeodeRobotState
          .orElse(newObsidianRobotState)
          .orElse(newClayRobotState)
          .orElse(newOreRobotState)*/
        // .getOrElse(stateWithAdditionalResources)

        val newStates = List(
          newGeodeRobotState.orElse(Some(stateWithAdditionalResources)),
          newObsidianRobotState,
          newClayRobotState,
          newOreRobotState
        ).flatten.filter { s =>
          visited.getOrElse(s.timeRemaining, 0) <= s.geodeRobots
        }
        pq.enqueue(newStates: _*)
        // pq.enqueue(build)

        loop(pq, visited + (state.timeRemaining -> state.geodeRobots))

    val startingState = State(1, 0, 0, 0, 0, 0, 0, 0, minutes)
    loop(mutable.PriorityQueue[State](startingState)(Ordering.by(s => (s.timeRemaining, s.geode))), Map.empty)

  def part1(lines: List[String]): Int =
    parse(lines).map(b => solve(b, 24) * b.number).sum

  def part2(lines: List[String]): Int =
    parse(lines).take(3).map(solve(_, 32)).foreach(println)
    0
