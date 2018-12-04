import strutils, times, algorithm, sequtils, math

type
  night = object
    guard: string
    date: DateTime
    asleepTimes: array[60, char]

type
  dataPoint = tuple[event: char, date: DateTime, guard: string]

type
  guardRecord = object
    guard: string
    asleepTimes: array[60, int]

proc parseRaw(entries: seq[string]): seq[dataPoint] =
  for line in entries:
    var dt = parse(line[1..16], "yyyy-MM-dd hh:mm")
    let event = line[19]
    #if event == 'G':
     # dt += 1.hours
    var guard = ""
    if line[25] == '#':
      guard = line.split(' ')[3]
    result.add((event: event, date: dt, guard: guard))

proc parseLines(entries: seq[string]): seq[night] =
  var lines = parseRaw(entries)
  var lastIndex = -1
  lines.sort do (x, y: dataPoint) -> int:
    result = cmp(x.date, y.date)
  for line in lines:
    if line.event == 'G':
      result.add(night(guard: line.guard, date: line.date + 1.hours))
      lastIndex += 1
    else:
      result[lastIndex].asleepTimes[parseInt(line.date.format("m"))] = line.event

proc allGuards(entries: seq[string]): seq[string] =
  var lines = parseRaw(entries)
  for line in lines:
    if not result.contains(line.guard):
      result.add(line.guard)

proc makeRecords(entries: seq[string]): seq[guardRecord] =
  let nights = parseLines(entries)
  for guard in allGuards(entries):
    var record = guardRecord(guard: guard)
    for night in nights.filter(proc (n: night): bool = n.guard == guard):
      var awake = true
      for i, e in night.asleepTimes:
        if e == 'f': 
          awake = false
        elif e == 'w':
          awake = true
        if not awake:
          record.asleepTimes[i] += 1
    result.add(record)

proc getAnswerOne(entries: seq[string]): int =
  var times: seq[tuple[minsAsleep: int, guard: string]] = @[]
  let records = makeRecords(entries)
  for record in records:
    times.add((minsAsleep: record.asleepTimes.sum, guard: record.guard))
  var maxTime = 0
  var sleepiestGuard = ""
  for guard in times:
    if guard.minsAsleep > maxTime:
      maxTime = guard.minsAsleep
      sleepiestGuard = guard.guard
  var sleepiestMinute = 0
  var mostSleepInMinute = 0
  for record in records:
    if record.guard == sleepiestGuard:
      for i, minute in record.asleepTimes:
        if minute > mostSleepInMinute:
          mostSleepInMinute = minute
          sleepiestMinute = i
  return sleepiestMinute * parseInt(sleepiestGuard[1..^1])

proc getAnswerTwo(entries: seq[string]): int =
  let records = makeRecords(entries)
  var maxMinute = 0
  var maxAsleepInMinute = 0
  var sleepiestGuard = ""
  for record in records:
    for i, minute in record.asleepTimes:
      if minute > maxAsleepInMinute:
        maxAsleepInMinute = minute
        maxMinute = i
        sleepiestGuard = record.guard

  return maxMinute * parseInt(sleepiestGuard[1..^1])


echo getAnswerOne(readFile("day04input.txt").splitLines)

echo getAnswerTwo(readFile("day04input.txt").splitLines)