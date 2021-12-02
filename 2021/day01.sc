val lines = scala.io.Source.fromFile("./input/day01/aoc_2021_day_1_input.txt").getLines.toList
lines.map(i => i.toInt).sliding(2).map(l => l(1) - l(0)).filter(i => i > 0).length 
//1462
lines.map(i => i.toInt).sliding(3).map(l => l(0)+ l(1) + l(2)).sliding(2).map(l => l(1) - l(0)).filter(i => i > 0).length  
//1497
