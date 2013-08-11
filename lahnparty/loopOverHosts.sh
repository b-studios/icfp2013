#!/bin/sh -x

id=1

# XXX add more hosts
hosts="sakania boma alexandria"

for name in ${hosts}; do
    ssh -n $name icfp2013/lahnparty/monitorRun.sh ${id} &
    let id++
done
