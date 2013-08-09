#
#

id = %Q{
MFrVnSUaIMxUZ38ZDqBzwkwz
}.strip

program = %Q{
(lambda (x) (shr4 (shr16 (not x))))
}.strip

guessReq = %Q[
{"id":"#{id}",
"program":"#{program}"}
]

puts guessReq
