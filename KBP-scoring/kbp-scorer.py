
from __future__ import division
import sys
import re

"""
Scores KBP results.

The scoring is done in terms of: 
query id, query entity, slotname, slotfill

This scorer puts these entities into tuples and uses direct string comparison.
"""


"""
Loads the results file.

Ignores any lines where the first entry of the line is not in the form of a 
query ID (SFXX_ENG_XXX).

Each result line must start with 5 tab separated entries, in which
the first second, fourth, and fifth entries must be the
query id, query entity, slotname and slotfill, respectively.
The third entry can be anything, as it is ignored.

@returns: a tuple of the set of results tuples, 
          a map from result tuple to the original line in the results file,
          and the ignored lines.
"""
def load_results(filename):
  valid_query_id = re.compile(r"SF[0-9][0-9]_ENG_[0-9][0-9][0-9]") 
  query_id_length = 12

  lines = file(filename, 'r').read().splitlines()
  tokenized = [(line.split('\t'), line) for line in lines]

  ignored = []
  results = []
  result_to_line_map = {}
  for tokens, line in tokenized:
    # Ignore if the first token isn't a query id.
    if not (re.findall(valid_query_id, tokens[0])\
        and len(tokens[0]) == query_id_length):
      ignored.append(line)
      continue

    # Create result tuple and add an entry to the map.
    #result = (tokens[0], tokens[1].lower(), tokens[3].lower(), tokens[4].lower()) 
    result = (tokens[0], tokens[3].lower(), tokens[4].lower()) 
    
    # Merged
    #result = (tokens[0], tokens[3], tokens[4]) 
    result_to_line_map[result] = line
    results.append(result)

  return (set(results), result_to_line_map, ignored)


"""
Answer key file formats are tab delimited,
query id, query entity, slotname, slotfill

@returns two sets, one of the correct and on of the inexact.
"""
def load_answer_keys(year):
  def setify_file(filename):
    lines = file(filename, 'r').read().splitlines()
    tupled = [tuple(line.split('\t')) for line in lines]
    #tupled = [(t[0], t[1].lower(), t[2].lower(), t[3].lower()) for t in tupled] 
    tupled = [(t[0], t[2].lower(), t[3].lower()) for t in tupled] 
    
    return set(tupled)

  """
  Original
  answerkey_dir = "../KBP-answerKeys"
  correct_template = "correct_KBP{}.txt"
  inexact_template = "inexact_KBP{}.txt"
  """
  #imple_eval_formatted
  answerkey_dir = "../KBP-answerKeys/implie_eval_formatted"
  correct_template = "correct_KBP{}_implie.csv"
  inexact_template = "inexact_KBP{}_implie.csv"

  return (setify_file(answerkey_dir + "/" + correct_template.format(year)),\
      setify_file(answerkey_dir + "/" + inexact_template.format(year)))


if __name__ == "__main__":
  if len(sys.argv) < 4:
    sys.exit("Usage: python kbp-scorer.py [results file] [year] [output file]")
  
  # Load files into sets.
  resultset, result_to_line_map, ignored = load_results(sys.argv[1])
  correctset, inexactset = load_answer_keys(sys.argv[2])
  
  # Find the correct, inexact and incorrect.
  correct = resultset & correctset
  inexact = (resultset & inexactset) - correct
  incorrect = resultset - (correctset | inexactset)

  # Calculate precision, recall
  precision = len(correct) / max(1, len(resultset))
  inexact_precision = (len(correct) + len(inexact)) / max(1, len(resultset))
  recall = len(correct) / max(1, len(correctset))
  inexact_recall = (len(correct) + len(inexact)) / max(1, (len(correctset) + len(inexactset)))
  
  # Output results.
  out = file(sys.argv[3], 'w')
  out.write("Ignored lines from the results file.\n")
  out.write("\n".join(ignored))
  out.write("\n")
  
  correctlines = [result_to_line_map[result] for result in correct]
  inexactlines = [result_to_line_map[result] for result in inexact]
  incorrectlines = [result_to_line_map[result] for result in incorrect]
  out.write("Correct results\n")
  out.write("\n".join(sorted(correctlines)))
  out.write("\n\n\n")
  out.write("Inexact results\n")
  out.write("\n".join(sorted(inexactlines)))
  out.write("\n\n\n")
  out.write("Incorrect results\n")
  out.write("\n".join(sorted(incorrectlines)))
  out.write("\n\n\n")

  out.write("Result statistics\n")
  out.write("Precision:\t{}\n".format(precision))
  out.write("Recall:\t{}\n".format(recall))
  out.write("With inexact results:\n")
  out.write("Precision:\t{}\n".format(inexact_precision))
  out.write("Recall:\t{}\n".format(inexact_recall))
  out.close()
