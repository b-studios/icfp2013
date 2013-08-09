#
# script to generate eval request

id = %Q{
cVBdX88Lz74jTfLTSj2YseZW
}.strip

evalReq = %Q[
{"id":"#{id}",
"arguments":["0xFFFFFFFFFFFFFFFF"]}
]

puts evalReq
