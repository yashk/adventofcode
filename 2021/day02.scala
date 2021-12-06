# part one
val lines = scala.io.Source.fromFile("./input/day02/aoc_2021_day_2_input.txt").getLines.toList
val parsed = lines.map(s => {
    val t = s.split(" ")
    (t(0), t(1).toInt)
}).map(
    tuple => tuple match {
        case ("forward", h) => (h, 0)
        case ("down", d) => (0, d)
        case ("up", d) => (0, -d)
    }
)

val finalPos = parsed.foldLeft(0,0)((b, a) => (b._1 + a._1, b._2 + a._2 ))
val partOneAns = finalPos._1 * finalPos._2


// convert to (command:String, x:Int) tuple
val parsed = scala.io.Source.fromFile("./input/day02/aoc_2021_day_2_input.txt").getLines.
map(s => {
    val t = s.split(" ")
    (t(0), t(1).toInt)
}).toList

// foldleft B = (aim, horizontal, depth) , A = (command,x)
//
val r = parsed.foldLeft(0,0,0){
    case ((aim, horizontal, depth), (command,x)) => {
        command match {
           case "forward" =>  (aim, horizontal + x, depth + (aim * x)) 
           case "down" => (aim + x, horizontal, depth) 
           case "up" => (aim - x, horizontal, depth) 
        }
    }
}

//r: (Int, Int, Int) = (916, 1970, 1000556)

val partTwoAns = r._2 * r._3
//partTwoAns: Int = 1971095320


