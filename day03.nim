import strutils, sequtils

type
  swatch = tuple[id: int, x: int, y: int, w: int, h: int]

type
  swatchCollection = tuple[rightMost: int, downMost: int, swatches: seq[swatch]]

proc processSwatch(s: string): swatch =
  var params = s.split({'x','#',' ','@',':',','})
    .filter(proc(s: string): bool = s != "")
    .map(parseInt)
  return (id: params[0], x: params[1], y: params[2], w: params[3], h: params[4])

proc processSwatches(rawData: seq[string]): swatchCollection =
  var swatches = rawData.map(proc(s: string): swatch = processSwatch(s))
  var 
    rightMost= max(swatches.map(proc(s: swatch): int = s.x + s.w))
    downMost= max(swatches.map(proc(s: swatch): int = s.y + s.h))
  return (rightMost: rightMost, downMost: downMost, 
    swatches: swatches)

proc countAll(swatches: swatchCollection): seq[int] =
  var countArray = repeat(0, swatches.rightMost * swatches.downMost)
  for s in swatches.swatches:
    for i in 0 ..< s.w:
      for j in 0 ..< s.h:
        countArray[s.x + i + swatches.rightMost * (s.y + j)] += 1
  return countArray

proc countDupes(rawData: seq[string]): int =
  var dupes = countAll(processSwatches(rawData))
  return len(dupes.filter(proc(c: int): bool = c > 1))


proc findIntact(rawData: seq[string]): int =
  var
    swatches = processSwatches(rawData)
    dupes = countAll(swatches)
  for s in swatches.swatches:
    var intact = true
    for i in 0 ..< s.w:
      for j in 0 ..< s.h:
        if dupes[s.x + i + swatches.rightMost * (s.y + j)] != 1:
          intact = false
        if not intact:
          break
      if not intact:
        break
    if intact:
      return s.id
    

echo countDupes(readFile("day03input.txt").splitLines)
echo findIntact(readFile("day03input.txt").splitLines)