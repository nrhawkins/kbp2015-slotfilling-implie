#!/bin/bash
FREEBASE="/projects/WebWare5/multir-multilingual/data/freebase/freebase-rdf-2014-04-06-00-00"
#OUTPUTDIR="/projects/WebWare5/multir-multilingual/data"
OUTPUTDIR="/homes/gws/genelkim/freebase"

#Getting both common.topic.alias and type.object.name returned too many results for
#MapBasedEntityLinker to handle, so just getting common.topic.alias for now
RELATIONS="(common\.topic\.alias)"
#RELATIONS="(common\.topic\.alias|type\.object\.name)"

#FREEBASEFORMAT="<http:\/\/rdf\.freebase\.com\/ns\/([^>]+)>\s+<http:\/\/rdf\.freebase\.com\/ns\/${RELATIONS}>\s+\"([^\"]*)\".*"
#Use following form to limit to one language (e.g., en)
FREEBASEFORMAT="<http:\/\/rdf\.freebase\.com\/ns\/([^>]+)>\s+<http:\/\/rdf\.freebase\.com\/ns\/${RELATIONS}>\s+\"([^\"]*)\"@en.*"

sed -rn "s/${FREEBASEFORMAT}/\1\t\3/p" ${FREEBASE} > "${OUTPUTDIR}/entityAliases"

exit 0 
