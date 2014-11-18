package testers

import java.io.PrintWriter

/**
 * A class to hold all the information for a test run,
 * through ModularTestRunner.
 *
 * Really, it's just a struct.
 * I've been doing a lot of C programming recently...
 *
 * @author Gene Kim (genelkim@cs.washington.edu)
 */
class TestInfo[T1, T2, T3](
    _function: T1 => T2,
    _inputs: Iterable[T1],
    _solutions: Iterable[T3],
    _comparator: ((T2, T3)) => (TestResults, String),
    _output: PrintWriter,
    _header: String = "",
    _footer: String = "") {
  def function = _function
  def inputs = _inputs
  def solutions = _solutions
  def comparator = _comparator
  def output = _output
  def header = _header
  def footer = _footer
}