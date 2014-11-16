package testers

import java.io.PrintWriter

/**
 * Created by Gene on 11/16/2014.
 */
class TestInfo[T1, T2](
    _function: T1 => T2,
    _inputs: Iterable[T1],
    _expected: Iterable[T2],
    _comparator: ((T2, T2)) => (TestResults, String),
    _output: PrintWriter,
    _header: String = "",
    _footer: String = "") {
  def function = _function
  def inputs = _inputs
  def expected = _expected
  def comparator = _comparator
  def output = _output
  def header = _header
  def footer = _footer
}