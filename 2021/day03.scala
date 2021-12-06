
val testInput = """00100
11110
10110
10111
10101
01111
00111
11100
10000
11001
00010
01010
""".split("\\n").toList



val testInputSmall = """11110
10110
""".split("\\n").toList



def nthBitSet(bitString:String, n:Int): Int = {
    (bitString.charAt(n).toInt - 48)
}


def frequency(lines:List[String],bitPos:Int, frequent: ((Int,Int)) => Int) = {
    val r = lines.map(
        line => line.charAt(bitPos).toInt - 48
    ).foldLeft(0,0){
        case((zero,one),x) => {
            x match {
                case 1 =>  (zero,one + 1)
                case 0 =>  (zero + 1, one)
            }
        }
    }

    frequent(r)
}

val frequent: ((Int,Int)) => Int = {
    case((zero,one)) => {
        (zero,one) match {
            case x if(zero > one) => 0
            case x if(zero < one) => 1
            case _ => Integer.MAX_VALUE
        }
    }
}

val less_frequent: ((Int,Int)) => Int = {
    case((zero,one)) => {
        (zero,one) match {
            case x if(zero > one) => 1
            case x if(zero < one) => 0
            case _ => Integer.MAX_VALUE
        }
    }
}


frequency("11110, 10110, 10111, 10101, 11100, 10000, 11001".split(", ").toList,1,frequent) 



def o2(lines:List[String], bitPos:Int): Int = {
    if(lines.size == 1){
        Integer.parseInt(lines(0),2)
    } else {
        val f = frequency(lines,bitPos,frequent)
        val filtered_lines = lines.filter(line => {
            val n = nthBitSet(line,bitPos)
            (f,n) match{
                case x if(f == Integer.MAX_VALUE && n == 1) => true
                case x if(f == n) => true
                case _ => false
            }
        })

        //println(s"lines=${lines} bitPos=${bitPos} freq=${freq.toList} f=${f} flines=${filtered_lines} size=${filtered_lines.length}")
        o2(filtered_lines, bitPos + 1)
    }
}


def co2(lines:List[String], bitPos:Int): Int = {
    if(lines.size == 1){
        Integer.parseInt(lines(0),2)
    } else {
        val f = frequency(lines,bitPos,less_frequent)
        val filtered_lines = lines.filter(line => {
            val n = nthBitSet(line,bitPos)
            (f,n) match{
                case x if(f == Integer.MAX_VALUE && n == 0) => true
                case x if(f == n) => true
                case _ => false
            }
        })

        //println(s"lines=${lines} bitPos=${bitPos} freq=${freq.toList} f=${f} flines=${filtered_lines} size=${filtered_lines.length}")
        co2(filtered_lines, bitPos + 1)
    }
}








val actualInput = scala.io.Source.fromFile("./input/day03/input.txt").getLines.toList



val rows = 2
val cols = 12
val a = Array.ofDim[Byte](rows, cols)
val lines = scala.io.Source.fromFile("./input/day03/input.txt").getLines.toList
val length  = lines.length 
val a = new Array[Int](12)

val r = lines.foldLeft(a){
    case (a, reading) => {
        val t = reading.toArray.map(_.toInt - 48)
        t.zip(a).map{ case (a,b) => a+b }
    }
}

val freq = r.map( i => ((length/2) - i).signum)



val gamma = Integer.parseInt(r.map(i => if(i > length/2) "1" else "0").mkString,2)
val epsilon =  Integer.parseInt(r.map(i => if(i < length/2) "1" else "0").mkString,2)

val finalResult = gamma * epsilon 
// res69: Int = 2583164





def o2(lines:List[String], bitPos:Int, frequency:Array[Int]): Int = {
    if(lines.size == 1){
        Integer.parseInt(lines(0),2)
    } else {
        val f = frequency(bitPos)
        val filtered_lines = lines.filter(line => {
            val n = nthBitSet(line,bitPos)
            (f,n) match {
                // f=-1 => 0, f=1 => 1 f=0 f(1) == f(0)
                case (-1, 0) => true
                case (1, 1) => true
                case (0, 1) => true
                case (_,_) => false
            }
        })

        println(s"f=${f} flines=${filtered_lines} size=${filtered_lines.length}")
        func(filtered_lines, bitPos + 1, frequency)
    }
}

def co2(lines:List[String], bitPos:Int, frequency:Array[Int]): Int = {
    if(lines.size == 1){
        Integer.parseInt(lines(0),2)
    } else {
        val f = frequency(bitPos)
        val filtered_lines = lines.filter(line => {
            val n = nthBitSet(line,bitPos)
            (f,n) match {
                // f=-1 => 0, f=1 => 1 f=0 f(1) == f(0)
                case (-1, 0) => false
                case (1, 1) => false
                case (0, 1) => false
                case (_,_) => true
            }
        })

        //println(s"f=${f} flines=${filtered_lines} size=${filtered_lines.length}")
        func(filtered_lines, bitPos + 1, frequency)
    }
}

oxygen(testInput, 0, freq)



1270 * 3315