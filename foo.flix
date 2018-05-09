// Hvordan kan vi lave store lister ?
// Hvordan tilgår vi lister?

def sumArray(array: Array[Int], start: Int, end: Int) : Int = {
  let sum = calculateSum(array, start, end);
  sum
}

def calculateSum(array: Array[Int], start: Int, end: Int) : Int = {
   if(start == end)
       array[start]
   else
     array[start] + calculateSum(array, start + 1, end)
}

/*
def sumList(list: List[Int]) : Int = {
  if(//List first == Nil)
    0
  else
    sumList(//currentElement + sum rest of list)
}
*/

@benchmark
def benchmark1(): Int = sumArray([10;2], 0, 1)
@benchmark
def benchmark2(): Int = sumArray([10;4], 0, 3)
@benchmark
def benchmark3(): Int = sumArray([10;8], 0, 7)
@benchmark
def benchmark4(): Int = sumArray([10;16], 0, 15)
@benchmark
def benchmark5(): Int = sumArray([10;32], 0, 31)
@benchmark
def benchmark6(): Int = sumArray([10;64], 0, 63)
@benchmark
def benchmark7(): Int = sumArray([10;128], 0, 127)
@benchmark
def benchmark8(): Int = sumArray([10;256], 0, 255)
@benchmark
def benchmark9(): Int = sumArray([10;512], 0, 511)
@benchmark
def benchmark10(): Int = sumArray([10;1024], 0, 1023)
@benchmark
def benchmark11(): Int = sumArray([10;2048], 0, 2047)
@benchmark
def benchmark12(): Int = sumArray([10;4096], 0, 4095)
@benchmark
def benchmark13(): Int = sumArray([10;8192], 0, 8191)
@benchmark
def benchmark14(): Int = sumArray([0;16384], 0, 16383)
@benchmark
def benchmark15(): Int = sumArray([10;32768], 0, 32767)
@benchmark
def benchmark16(): Int = sumArray([10;65536], 0, 65535)
@benchmark
def benchmark17(): Int = sumArray([10;131072], 0, 131071)
@benchmark
def benchmark18(): Int = sumArray([10;262144], 0, 262143)
@benchmark
def benchmark19(): Int = sumArray([10;524288], 0, 524287)












/*
def binarySearch(array : Array[Int], start: Int, end: Int, element : Int) : Int = {

   if(end >= start)
     let mid = start + (end - start) / 2;

     if(element == array[mid])
       mid
     else if(element < array[mid])
       binarySearch(array, start, mid - 1, element)
     else
       binarySearch(array, mid + 1, end, element)
   else
      -1
}

@benchmark
def benchmark1(): Int = binarySearch([10;2], 0, 1, 139)
@benchmark
def benchmark2(): Int = binarySearch([10;4], 0, 3, 139)
@benchmark
def benchmark3(): Int = binarySearch([10;8], 0, 7, 139)
@benchmark
def benchmark4(): Int = binarySearch([10;16], 0, 15, 139)
@benchmark
def benchmark5(): Int = binarySearch([10;32], 0, 31, 139)
@benchmark
def benchmark6(): Int = binarySearch([10;64], 0, 63, 139)
@benchmark
def benchmark7(): Int = binarySearch([10;128], 0, 127, 139)
@benchmark
def benchmark8(): Int = binarySearch([10;256], 0, 255, 139)
@benchmark
def benchmark9(): Int = binarySearch([10;512], 0, 511, 139)
@benchmark
def benchmark10(): Int = binarySearch([10;1024], 0, 1023, 139)
@benchmark
def benchmark11(): Int = binarySearch([10;2048], 0, 2047, 139)
@benchmark
def benchmark12(): Int = binarySearch([10;4096], 0, 4095, 139)
@benchmark
def benchmark13(): Int = binarySearch([10;8192], 0, 8191, 139)
@benchmark
def benchmark14(): Int = binarySearch([10;16384], 0, 16383, 139)
@benchmark
def benchmark15(): Int = binarySearch([10;32768], 0, 32767, 139)
@benchmark
def benchmark16(): Int = binarySearch([10;65536], 0, 65535, 139)
@benchmark
def benchmark17(): Int = binarySearch([10;131072], 0, 131071, 139)
@benchmark
def benchmark18(): Int = binarySearch([10;262144], 0, 262143, 139)
@benchmark
def benchmark19(): Int = binarySearch([10;524288], 0, 524287, 139)
@benchmark
def benchmark20(): Int = binarySearch([10;1048576], 0, 1048575, 139)
@benchmark
def benchmark21(): Int = binarySearch([10;2097152], 0, 2097151, 139)
@benchmark
def benchmark22(): Int = binarySearch([10;4194304], 0, 4194303, 139)
@benchmark
def benchmark23(): Int = binarySearch([10;8388608], 0, 8388607, 139)
*/

