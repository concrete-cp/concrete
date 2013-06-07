package javax.constraints.impl;

class VarBool(problem: Problem, name: String) extends Var(problem, name, problem.cspom.boolVar(name))
  with javax.constraints.VarBool;