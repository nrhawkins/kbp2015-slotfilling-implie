#!/bin/bash

sbt "run-main
tac.RunKBP2015ImplieGroupQueriesCorpusSerialized
/homes/gws/nhawkins/kbp2015-slotfilling-implie/kbp/queries/queries2015_r1.xml cs 
/homes/gws/nhawkins/kbp2015-slotfilling-implie/kbp/reldocs2015/relDocs_queries2015_r1
/homes/gws/nhawkins/kbp2015-slotfilling-implie/round1/out/out_queries2015_r1_4 round1 74 371 
/projects/WebWare6/KBP_2015/corpus/serialized_corenlp_3.5/"

exit 0
