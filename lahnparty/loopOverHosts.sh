#!/bin/sh -x

id=1

# XXX add more hosts
#hosts="sakania boma alexandria"
# Sorted in order of decreasing memory size.
hosts="sakania
boma
kumasi
daloa
balaka
pretoria
alexandria
laayoune
monrovia
kisumu
lukala
banda
nara
suez
diffa
bana
karoma"

for name in ${hosts}; do
    ssh -n $name icfp2013/lahnparty/monitorRun.sh ${id} &
    let id++
done
