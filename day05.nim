import strutils, sequtils, unicode

proc numberOfUnits(text: string): int =
  var 
    repeat = false
    i = 0
    newString = ""
  while i < text.len - 1:
    if text[i].toLowerAscii == text[i+1].toLowerAscii and (text[i].isLowerAscii xor text[i+1].isLowerAscii):
      repeat = true
      i += 2
    else:
      newString.add(text[i])
      i += 1
  newString.add(text[^1])
  if repeat:
    return numberOfUnits(newString)
  else:
    return newString.len

proc findUnitToRemove(text: string): int =
  var
    shortest = 10000000
  for s in 'A'..'Z':
    var str = ""
    str.add(s)
    let newTotal = numberOfUnits(text.multiReplace((str, ""),(str.toLowerAscii, "")))
    if newTotal < shortest:
      shortest = newTotal
  return shortest
    

echo numberOfUnits(readFile("day05input.txt"))
echo findUnitToRemove(readFile("day05input.txt"))