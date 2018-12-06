import strutils, sequtils, tables

type Matrix = seq[seq[int]]
type Point = seq[int]

func constructMatrix(pointStrings: seq[string]): Matrix =
  let points = pointStrings.mapIt(it.split(',').mapIt(it.strip().parseInt()))
  let maxX = points.mapIt(it[0]).max + 1
  let maxY = points.mapIt(it[1]).max + 1
  result = @[]
  for i in 1 .. maxX:
    result &= newSeq[int](maxY)
  var ident = 1
  for point in points:
    result[point[0]][point[1]] = ident
    ident += 1

func manhattanDistance(a, b: Point): int =
  abs(a[0] - b[0]) + abs(a[1] - b[1])

proc findSizes(pointStrings: seq[string]): int =
  let 
    points = pointStrings.mapIt(it.split(',').mapIt(it.strip().parseInt()))
    maxX = points.mapIt(it[0]).max + 1
    maxY = points.mapIt(it[1]).max + 1
  var sizes = initCountTable[char]()
  var prohibited: seq[char] = @[chr(150)]
  for i in 0 .. maxY:
    for j in 0 .. maxX:
      var minDistance = maxX + maxY
      var minPoint = 100
      for idx, p in points:
        let dist = manhattanDistance(@[i,j], p)
        if dist < minDistance:
          minPoint = idx
          minDistance = dist
          # echo i, ' ', j, ' ', minPoint, ' ', minDistance
        elif dist == minDistance:
          minPoint = 150
      if i == 0 or j == 0 or i == maxY or j == maxX:
        prohibited &= chr(minPoint)
      sizes.inc(chr(minPoint))
  sizes.sort()
  for k, v in sizes.pairs():
    if not prohibited.contains(k):
      return v
  
proc findRegion(pointStrings: seq[string]): int =
  let 
    points = pointStrings.mapIt(it.split(',').mapIt(it.strip().parseInt()))
    maxX = points.mapIt(it[0]).max + 1
    maxY = points.mapIt(it[1]).max + 1
  for i in 0 .. maxY:
    for j in 0 .. maxX:
      var totalDist = 0
      for idx, p in points:
        totalDist += manhattanDistance(@[i,j], p)
      if totalDist < 10000:
        result += 1

echo findSizes(readfile("day06input.txt").splitLines)
echo findRegion(readfile("day06input.txt").splitLines)