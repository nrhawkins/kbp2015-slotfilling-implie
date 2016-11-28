#!/bin/bash

sbt "run-main
tac.RunKBP2015ImplieGroupQueriesCorpusSerialized
/homes/gws/nhawkins/kbp2015-slotfilling-implie/kbp/queries/queries2015_r2.xml cs 
/homes/gws/nhawkins/kbp2015-slotfilling-implie/kbp/reldocs2015/relDocs_queries2015_r2
/homes/gws/nhawkins/kbp2015-slotfilling-implie/round2/out/out_implie_r2_6 round2 908 311 
/projects/WebWare6/KBP_2015/corpus/serialized_corenlp_3.5/"

exit 0
