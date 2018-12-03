import strutils, sequtils

const input = readFile("day02input.txt")

proc twosAndThrees(str: string): tuple[two: bool, three: bool] =
  result = (two: false, three: false)
  for c in str:
    if count(str, c) == 2:
      result.two = true
    if count(str, c) == 3:
      result.three = true

proc checkSum(ids: seq[string]): int =
  var twos = 0
  var threes = 0
  for id in ids:
    var res = twosAndThrees(id)
    if res.two:
      twos += 1
    if res.three:
      threes += 1
  result = twos * threes

proc findCommon(ids: seq[string]): string =
  var res: seq[string] = @[]
  for id in ids:
    for i, c in id:
      if i > 0 and id[i - 1] != c:
        var idTransform = id[0 ..< i] & id[i + 1 .. id.len - 1]
        if res.contains(idTransform):
          return idTransform
        res.add(idTransform)

echo findCommon(input.splitLines)

echo checkSum(input.splitLines)
