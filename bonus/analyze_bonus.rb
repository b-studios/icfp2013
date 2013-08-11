
# first column is the problem id
matrix = File.read('bonus_problems_table').split("\n").map {|line| line.split(" ") }


puts matrix.map { |p1|

  found_column = nil
  
  (1 .. p1.size - 1).each do |columnIndex|
    column = matrix.map {|r| r[columnIndex] }
    found_column = columnIndex if found_column == nil and column.count(p1[columnIndex]) == 1
  end

  p1[0].to_s + " " + found_column.to_s
}.join("\n")
