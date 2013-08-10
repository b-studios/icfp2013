require 'thin'

rackup_file = "server.rb" 

argv = ARGV
argv << ["-R", rackup_file ] unless ARGV.include?("-R")
argv << ["-e", "production"] unless ARGV.include?("-e")

puts argv.flatten

Thin::Runner.new(argv.flatten).run!