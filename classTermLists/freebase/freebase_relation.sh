#!/bin/bash
FREEBASE="/projects/WebWare5/multir-multilingual/data/freebase/freebase-rdf-2014-04-06-00-00"
#OUTPUTDIR="/projects/WebWare5/multir-multilingual/data/relationFiles/"
OUTPUTDIR="/homes/gws/genelkim/freebase/"
relations=( people.person.profession
						#people.person.spouse_s
            #people.person.schools
            #people.person.place_of_birth
            #.city
            #film.film.directed_by
            #organization.organization.founders
            #people.person.children
            #people.person.parents
)

for PATTERN in ${relations[@]}; do
    FREEBASEFORMAT="<http:\/\/rdf\.freebase\.com\/ns\/([^>]+)>\s+<http:\/\/rdf\.freebase\.com\/ns\/(${PATTERN})>\s+<http:\/\/rdf\.freebase\.com\/ns\/([^>]+)>\s+\."
    echo grepping $PATTERN
    echo outputting results to $OUTPUTDIR/freebase-$PATTERN
    sed -rn "s/${FREEBASEFORMAT}/\1\t\2\t\3/p" ${FREEBASE} > ${OUTPUTDIR}/freebase-$PATTERN
done
exit 0 
