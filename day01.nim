import strutils, macros

let nums = readFile("day01input.txt")

#Wow this is massively inefficient
#Calculates the first repeated number on a cyclical series of additions and subtractions
#from zero
proc calc(arr: seq[string]): int =
  var visited = @[0]
  while true:
    for x in arr:
      if x[0] == '+':
        result += parseInt(x[1 ..< x.len])
      else:
        result -= parseInt(x[1 ..< x.len])
      if visited.contains(result):
          return result
      visited.add(result)
  
    

echo calc(nums.splitLines)
