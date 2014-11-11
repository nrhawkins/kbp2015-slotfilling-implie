/**
 * Test result counter.  Has modifiable correct, incorrect and missing fields.
 * Default constructs sets all fields to zero.
 * Includes basic calculations: precison and recall.
 * TODO: add any other relevant calculations.
 * @author Gene Kim (genelkim@cs.washington.edu)
 *
 * @param c Number of correct tags.
 * @param i Number of incorrect tags.
 * @param m Number of missing tags.
 */
class TestResults(c: Int = 0, i: Int = 0, m: Int = 0) {
  var correct = c
  var incorrect = i
  var missed = m

  def precision = correct.toDouble / (correct + incorrect)
  def recall = correct.toDouble / (correct + missed)
}
