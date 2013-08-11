require_relative "parse.rb"

probs = File.read("progs30").split("\n").map {|p| /([^ ]*) (.*)/.match(p)[2] }

KEYS     = ["not", "shl1", "shr1", "shr4", "shr16", "and", "or", "xor", "plus" , "one", "zero", "fold", "if0", "input", "acc", "byte"]

stats = probs.inject(StatsParser.new "") {| memo, prob | memo + (StatsParser.new prob) }.stats 

def normalize(num, total)
  num.to_s
  format("%.3f", num.to_f / total)
end

stats.each do |op, data| 

	File.write("stats/#{op}", KEYS.map { |key| [key, normalize(data["general"][key], probs.size), normalize(data["direct"][key], probs.size)].join("\t") }.join("\n"))
end