#
# generate vectors with 1 bit of 1 and all others are 0

$format = "0x%016X"
$bits = (0..63).map{ |n| 1 << n }

$preexist = 2 + 64 # all-0, all-1 and 1 in each position
$total = 256 # max number of allowed data points in 1 request
$numberOfRandoms = $total - $preexist

$maxInt = $bits.inject{ |x, y| x + y }

$randoms = (0...$numberOfRandoms).map { rand($maxInt) }

$arguments = [0] + [0xFFFFFFFFFFFFFFFF] + $bits + $randoms

def toString(numVec)
  strings = numVec.map { |x| $format % x }
  strings.map{ |s| %Q{"#{s}"} }.join(",\n")
end

$request = %Q{
{"program":"(lambda (x) (#{ARGV[0]} x (shr16 x)))",
"arguments":[
#{toString($arguments)}
]}
}

puts $request
