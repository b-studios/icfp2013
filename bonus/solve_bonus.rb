#!/usr/bin/ruby

require "rubygems"
require "uri"
require "net/http"
require "json"


def secret
  '02768XDijvjky5OOedNdAnRxokV6hSA8aaFT1doK'
end

def performRequest(path, payload = nil)
  payload = JSON.generate(payload) unless payload == nil

  puts payload
  http = Net::HTTP.new(uri.host, uri.port)    
  handleResponse http.post(buildPath(path), payload)
end

def uri
  URI('http://icfpc2013.cloudapp.net')
end

def buildPath(path)
  "/#{path}?auth=#{secret}vpsH1H"
end

problems = File.read("bonus_problems_unique_with_id").split("\n").map {|p| 
  m = /([^ ]*) (.*)/.match(p)
  {
    id: m[1],
    source: m[2]
  }
}

training_numbers = File.read("numbers").split("\n")
http = Net::HTTP.new(uri.host, uri.port)    

problems.each do |p|

  resp = nil

  puts "Dealing with #{p[:id]}"

  # now for each problem run eval with our training numbers
  begin
    resp = http.post(buildPath('eval'), JSON.generate({
      id: p[:id],
      arguments: training_numbers
    }))
    Kernel.sleep(3)

  end while resp.code != "200"

  p[:outputs] = JSON.parse(resp.body)["outputs"]

end

File.write("bonus_problems_table", problems.map do |p|
  p[:id] + " " + p[:outputs].join(" ")
end.join("\n"))

=begin
distinquishing = []

# now go through all rows and find a column that is unique to a problem
#
#               problem1  problem2  problem3
# 0x0000000001         0         1         0
# 0x0000000010
# 0x0000000100
# 0x0000001000
# ...
training_numbers.each_index do |i| 

  row = problems.map { |p| p[:outputs][i] }

  row.collect { |n| row.count(n) == 1 }

end
=end
