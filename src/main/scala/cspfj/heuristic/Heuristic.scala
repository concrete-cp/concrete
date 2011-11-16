package cspfj.heuristic;

import cspfj.Pair;
import cspfj.problem.Problem;

trait Heuristic {
  def selectPair(problem: Problem): Option[Pair]
  def compute(): Unit
}
