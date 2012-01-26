package cspfj.constraint



trait SimpleRemovals extends Constraint {

  private var removals = 0;

  def setRemovals(position: Int, value: Int) { removals = value }

  def fillRemovals(value: Int) { removals = value }

  def hasNoRemovals(reviseCount: Int) = removals < reviseCount

}