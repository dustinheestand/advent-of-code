import strutils, sequtils, tables

type Pair =
  object
    before, after: char

type Task =
  object
    name: char
    predecessors: string
    completionTime: int
    inProgress: bool
    complete: bool

const numWorkers = 5

func timeToComplete(c: char): int = ord(c) - 4

func taskIndex(c: char): int = ord(c) - 64

proc processInput(raw: seq[string]): seq[Pair] =
  raw.mapIt(Pair(before: it[5], after: it[36]))

proc orderSingle(pairs: seq[Pair]): string =
  var heirarchy = initTable[char, string]()
  for pair in pairs:
    if not heirarchy.hasKey(pair.before):
      heirarchy.add(pair.before, "")
    if not heirarchy.hasKey(pair.after):
      heirarchy.add(pair.after, "")
    heirarchy[pair.after] &= pair.before
  while heirarchy.len > 0:
    var toDelete: char
    for k, v in heirarchy.pairs:
      if v == "":
        result &= k
        toDelete = k
        heirarchy.del(k)
        break
    for k, v in heirarchy.pairs:
      heirarchy[k] = v.replace($toDelete, "")

proc orderMultiple(pairs: seq[Pair], n: int): string =
  var tasks: seq[Task] = @[]
  for c in 'A'..'Z':
    tasks &= Task(name: c)
  var time = 0 
  echo tasks
  while tasks.anyIt(not it.complete):
    for task in tasks:
      if task.predecessors == "":
        task.inProgress = true
        
    
echo orderSingle(processInput(readFile("day07input.txt").splitLines))
echo orderMultiple(processInput(readFile("day07input.txt").splitLines), numWorkers)