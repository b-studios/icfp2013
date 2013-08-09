#
# script to generate guess request

id = %Q{
cVBdX88Lz74jTfLTSj2YseZW
}.strip

program = %Q{
(lambda (x) x)
}.strip

guessReq = %Q[
{"id":"#{id}",
"program":"#{program}"}
]

puts guessReq
