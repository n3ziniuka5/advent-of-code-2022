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
    timeRemaining: Int,
    log: String
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

    def skipUntil(pred: State => Boolean): State = {
      @tailrec
      def loop(state: State): State =
        if (pred(state)) state
        else loop(state.skipTurns(1))

      loop(this)
    }

    def skipTurns(turns: Int): State = {
      this.copy(
        timeRemaining = timeRemaining - turns,
        ore = ore + (oreRobots * turns),
        clay = clay + (clayRobots * turns),
        obsidian = obsidian + (obsidianRobots * turns),
        geode = geode + (geodeRobots * turns),
        // log = state.log + s"\n minute ${minutes - state.timeRemaining + 1}"
      )
    }

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

  def keepFindingQuickestPath(blueprint: Blueprint, minutes: Int): Int =
    println(s"solving blueprint $blueprint with new implementation")

    def loop(state: List[State]): Int =
      val newStates = state.flatMap(quickestPathToGeodeRobot(blueprint, _))

      if (newStates.nonEmpty) {
        val maxTime      = newStates.maxBy(_.timeRemaining).timeRemaining
        val fastestPaths = newStates.filter(_.timeRemaining == maxTime)

        loop(fastestPaths)
      } else {
        val result = state.map { s =>
          (s.geode, s.geodeRobots, s.timeRemaining)
        }
        println(s"ALSO EMPTY, DEAD END $result")

        result.head._1 + (result.head._2 * result.head._3)
      }

    val startingState = State(1, 0, 0, 0, 0, 0, 0, 0, minutes, "")
    loop(List(startingState))

  def quickestPathToGeodeRobot(blueprint: Blueprint, initialState: State): List[State] =
    @tailrec
    def loop(pq: mutable.PriorityQueue[State], answers: List[State]): List[State] =
      if (pq.isEmpty) answers
      else
        val state = pq.dequeue()
        if (state.timeRemaining == 0) answers
        else if (state.timeRemaining <= answers.headOption.map(_.timeRemaining).getOrElse(0)) answers
        else
          val stateWithAdditionalResources = state.copy(
            timeRemaining = state.timeRemaining - 1,
            ore = state.ore + state.oreRobots,
            clay = state.clay + state.clayRobots,
            obsidian = state.obsidian + state.obsidianRobots,
            geode = state.geode + state.geodeRobots
          )

          val newGeodeRobotState =
            if (state.ore >= blueprint.geodeRobotCost.ore && state.obsidian >= blueprint.geodeRobotCost.obsidian)
              // println(s"got an answer at time ${state.timeRemaining}")
              Some(
                stateWithAdditionalResources.copy(
                  geodeRobots = state.geodeRobots + 1,
                  ore = stateWithAdditionalResources.ore - blueprint.geodeRobotCost.ore,
                  obsidian = stateWithAdditionalResources.obsidian - blueprint.geodeRobotCost.obsidian
                )
              )
            else None

          newGeodeRobotState match
            case Some(newState) => loop(pq, newState +: answers)
            case None =>
              val newObsidianRobotState =
                if (
                  blueprint.maxObsidianRobots > state.obsidianRobots && state.ore >= blueprint.obsidianRobotCost.ore && state.clay >= blueprint.obsidianRobotCost.clay
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
                  /*newObsidianRobotState.isEmpty &&*/ blueprint.maxClayRobots > state.clayRobots && state.ore >= blueprint.clayRobotCost
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
                  /*newObsidianRobotState.isEmpty &&*/ blueprint.maxOreRobots > state.oreRobots && state.ore >= blueprint.oreRobotCost
                )
                  Some(
                    stateWithAdditionalResources
                      .copy(
                        oreRobots = state.oreRobots + 1,
                        ore = stateWithAdditionalResources.ore - blueprint.oreRobotCost
                      )
                  )
                else None

              val newStates = List(
                Some(stateWithAdditionalResources),
                newObsidianRobotState,
                newClayRobotState,
                newOreRobotState
              ).flatten
              pq.enqueue(newStates: _*)

              loop(pq, answers)

    val manyAnswers =
      loop(mutable.PriorityQueue[State](initialState)(Ordering.by(s => (s.timeRemaining, s.geodeRobots))), List.empty)
    // println(s"got number of answers ${manyAnswers.size}")

    manyAnswers

  def solve(blueprint: Blueprint, minutes: Int): Int =
    println(s"solving blueprint - $blueprint with time $minutes")
    def loop(pq: mutable.PriorityQueue[State], visited: Map[Int, Int]): Int =
      val state = pq.dequeue()
      if (state.timeRemaining == 0)
        println(state.log)
        state.geode
      // else if (visited.getOrElse(state.timeRemaining, 0) > state.geode + 5) loop(pq, visited)
      else
        // println(state)
        /*val stateWithAdditionalResources = state.copy(
          timeRemaining = state.timeRemaining - 1,
          ore = state.ore + state.oreRobots,
          clay = state.clay + state.clayRobots,
          obsidian = state.obsidian + state.obsidianRobots,
          geode = state.geode + state.geodeRobots,
          log = state.log + s"\n minute ${minutes - state.timeRemaining + 1}"
        )*/

        val buildGeodeRobot = if (state.obsidianRobots > 0) {
          Some(
            state
              .skipUntil(_.canBuildGeodeRobot(blueprint))
              .skipTurns(1)
              .addGeodeRobot(blueprint)
          ).filter(_.timeRemaining > 0)

          /*val missingObsidian = math.max(0, blueprint.geodeRobotCost.obsidian - state.obsidian)
          val missingOre      = math.max(0, blueprint.geodeRobotCost.ore - state.ore)

          if (missingObsidian > 0 || missingOre > 0) {
            /*val obsidianTurns =
              if (math.max(missingObsidian, state.obsidianRobots) % state.obsidianRobots == 0)
                missingObsidian / state.obsidianRobots
              else missingObsidian / state.obsidianRobots + 1
            val oreTurns =
              if (math.max(missingOre, state.oreRobots) % state.oreRobots == 0) missingOre / state.oreRobots
              else missingOre / state.oreRobots + 1

            val toSkip = math.max(obsidianTurns, oreTurns) + 1
            Option.when(state.timeRemaining > toSkip)(
              skipTurns(state, toSkip).copy(geodeRobots = state.geodeRobots + 1)
            )*/
            None*/
          /*} else {
            Some(skipTurns(state, 1).addGeodeRobot(blueprint))
          }*/
        } else None

        val buildObsidianRobot = if (state.clayRobots > 0 && state.obsidianRobots < blueprint.maxObsidianRobots) {
          Some(state.skipUntil(_.canBuildObsidianRobot(blueprint)).skipTurns(1).addObsidianRobot(blueprint))
            .filter(_.timeRemaining > 0)

          /*val missingClay = math.max(0, blueprint.obsidianRobotCost.clay - state.clay)
          val missingOre  = math.max(0, blueprint.obsidianRobotCost.ore - state.ore)

          if (missingClay > 0 || missingOre > 0) {
            /*val clayTurns =
              if (missingClay % state.clayRobots == 0) missingClay / state.clayRobots
              else missingClay / state.clayRobots + 1
            val oreTurns =
              if (missingOre % state.oreRobots == 0) missingOre / state.oreRobots else missingOre / state.oreRobots + 1

            val toSkip = math.max(clayTurns, oreTurns) + 1
            Option.when(state.timeRemaining > toSkip)(
              skipTurns(state, toSkip).copy(obsidianRobots = state.obsidianRobots + 1)
            )*/
            None
          } else {
            Some(skipTurns(state, 1).addObsidianRobot(blueprint))
          }*/
        } else None

        val buildClayRobot = if (state.clayRobots < blueprint.maxClayRobots) {
          Some(
            state
              .skipUntil(_.canBuildClayRobot(blueprint))
              .skipTurns(1)
              .addClayRobot(blueprint)
          ).filter(_.timeRemaining > 0)

          /*val missingOre = math.max(0, blueprint.clayRobotCost - state.ore)

          if (missingOre > 0) {
            /*val oreTurn =
              if (missingOre % state.oreRobots == 0) missingOre / state.oreRobots
              else missingOre / state.oreRobots + 1

            val toSkip = oreTurn + 1
            Option.when(state.timeRemaining > toSkip)(
              skipTurns(state, toSkip).copy(clayRobots = state.clayRobots + 1)
            )*/
            None
          } else {
            Some(skipTurns(state, 1).addClayRobot(blueprint))
          }*/
        } else None

        val buildOreRobot = if (state.oreRobots < blueprint.maxOreRobots) {
          Some(state.skipUntil(_.canBuildOreRobot(blueprint)).skipTurns(1).addOreRobot(blueprint))
            .filter(_.timeRemaining > 0)

          /*val missingOre = math.max(0, blueprint.oreRobotCost - state.ore)

          if (missingOre > 0) {
            /*val oreTurn =
              if (missingOre % state.oreRobots == 0) missingOre / state.oreRobots
              else missingOre / state.oreRobots + 1

            val toSkip = oreTurn + 1
            Option.when(state.timeRemaining > toSkip)(
              skipTurns(state, toSkip).copy(clayRobots = state.clayRobots + 1)
            )*/
            None
          } else {
            Some(skipTurns(state, 1).addOreRobot(blueprint))
          }*/
        } else None

        val justWait = if (buildGeodeRobot.isEmpty && buildObsidianRobot.isEmpty && buildClayRobot.isEmpty) {
          Some(state.skipTurns(state.timeRemaining))
        } else None

        val newStates = List(buildGeodeRobot, buildObsidianRobot, buildClayRobot, buildOreRobot, justWait).flatten

        /*val newStates =
          if (state.ore >= blueprint.geodeRobotCost.ore && state.obsidian >= blueprint.geodeRobotCost.obsidian)
            List(
              stateWithAdditionalResources.copy(
                geodeRobots = state.geodeRobots + 1,
                ore = stateWithAdditionalResources.ore - blueprint.geodeRobotCost.ore,
                obsidian = stateWithAdditionalResources.obsidian - blueprint.geodeRobotCost.obsidian,
                log =
                  stateWithAdditionalResources.log + s" bought geode robot, have ${state.geodeRobots + 1} robots now, total collected ${stateWithAdditionalResources.geode}",
              )
            )
          else {
            if (state.oreRobots < blueprint.clayRobotCost) {
              if (state.ore >= blueprint.oreRobotCost)
                List(
                  stateWithAdditionalResources
                    .copy(
                      oreRobots = state.oreRobots + 1,
                      ore = stateWithAdditionalResources.ore - blueprint.oreRobotCost,
                      log = stateWithAdditionalResources.log + " bought ore robot",
                    )
                )
              else List(stateWithAdditionalResources)
            } else {

              val newObsidianRobotState =
                if (
                  blueprint.maxObsidianRobots > state.obsidianRobots && state.ore >= blueprint.obsidianRobotCost.ore && state.clay >= blueprint.obsidianRobotCost.clay
                )
                  Some(
                    stateWithAdditionalResources.copy(
                      obsidianRobots = state.obsidianRobots + 1,
                      ore = stateWithAdditionalResources.ore - blueprint.obsidianRobotCost.ore,
                      clay = stateWithAdditionalResources.clay - blueprint.obsidianRobotCost.clay,
                      log = stateWithAdditionalResources.log + " bought obsidian robot",
                    )
                  )
                else None

              val newOreRobotState =
                if (blueprint.maxOreRobots > state.oreRobots && state.ore >= blueprint.oreRobotCost)
                  Some(
                    stateWithAdditionalResources
                      .copy(
                        oreRobots = state.oreRobots + 1,
                        ore = stateWithAdditionalResources.ore - blueprint.oreRobotCost,
                        log = stateWithAdditionalResources.log + " bought ore robot",
                      )
                  )
                else None

              val newClayRobotState =
                if (blueprint.maxClayRobots > state.clayRobots && state.ore >= blueprint.clayRobotCost)
                  Some(
                    stateWithAdditionalResources
                      .copy(
                        clayRobots = state.clayRobots + 1,
                        ore = stateWithAdditionalResources.ore - blueprint.clayRobotCost,
                        log = stateWithAdditionalResources.log + " bought clay robot",
                      )
                  )
                else None

              List(
                newObsidianRobotState,
                newOreRobotState,
                newClayRobotState,
                Some(stateWithAdditionalResources)
              ).flatten
            }
          }*/

        /*val newGeodeRobotState =
          if (state.ore >= blueprint.geodeRobotCost.ore && state.obsidian >= blueprint.geodeRobotCost.obsidian)
            Some(
              stateWithAdditionalResources.copy(
                geodeRobots = state.geodeRobots + 1,
                ore = stateWithAdditionalResources.ore - blueprint.geodeRobotCost.ore,
                obsidian = stateWithAdditionalResources.obsidian - blueprint.geodeRobotCost.obsidian,
                log =
                  stateWithAdditionalResources.log + s" bought geode robot, have ${state.geodeRobots + 1} robots now, total collected ${stateWithAdditionalResources.geode}",
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
                clay = stateWithAdditionalResources.clay - blueprint.obsidianRobotCost.clay,
                log = stateWithAdditionalResources.log + " bought obsidian robot",
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
                  ore = stateWithAdditionalResources.ore - blueprint.oreRobotCost,
                  log = stateWithAdditionalResources.log + " bought ore robot",
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
                  ore = stateWithAdditionalResources.ore - blueprint.clayRobotCost,
                  log = stateWithAdditionalResources.log + " bought clay robot",
                )
            )
          else None

        val waitingState =
          if (newGeodeRobotState.isEmpty && newObsidianRobotState.isEmpty && newOreRobotState.isEmpty)
            Some(stateWithAdditionalResources)
          else None*/

        /*val build = newGeodeRobotState
          .orElse(newObsidianRobotState)
          .orElse(newClayRobotState)
          .orElse(newOreRobotState)*/
        // .getOrElse(stateWithAdditionalResources)

        /*val newStates2 = List(
          newGeodeRobotState,
          newObsidianRobotState,
          newClayRobotState,
          newOreRobotState,
          waitingState
        ).flatten.filter { s =>
          visited.getOrElse(s.timeRemaining, 0) <= s.geode + 5
        }*/
        pq.enqueue(newStates: _*)
        // pq.enqueue(build)

        loop(
          pq,
          visited /*+ (state.timeRemaining -> math.max(visited.getOrElse(state.timeRemaining, 0), state.geode))*/
        )

    val startingState = State(1, 0, 0, 0, 0, 0, 0, 0, minutes, "")
    loop(mutable.PriorityQueue[State](startingState)(Ordering.by(s => (s.timeRemaining, s.geode))), Map.empty)

  def part1(lines: List[String]): Int =
    // parse(lines).map(b => solve(b, 24) * b.number).sum
    val answers = parse(lines).map(b => (b, solve(b, 24)))
    answers.foreach(println)
    answers.map(a => a._1.number * a._2).sum

  def part2(lines: List[String]): Int =
    // parse(lines).take(3).map(solve(_, 32)).foreach(println)
    val answers = parse(lines).take(3).map(solve(_, 32))
    answers.foreach(println)
    answers.product
