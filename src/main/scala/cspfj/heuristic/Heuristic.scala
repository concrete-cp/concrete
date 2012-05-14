package cspfj.heuristic;

import cspfj.Pair;
import cspfj.Problem;

trait Heuristic {
  def selectPair(problem: Problem): Option[Pair]
  def compute(): Unit
}
