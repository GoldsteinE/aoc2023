import std/syncio
import std/cmdline
import std/options
import std/sequtils
import std/strutils
import std/strscans
import std/strformat
import std/enumerate

type Lens = tuple[label: string, strength: int]
type Boxes = array[256, seq[Lens]]

proc hash(part: string): uint8 =
  part.foldl((a + uint8(b)) * 17, uint8(0))

proc findLens(boxes: Boxes, label: string): (uint8, Option[int]) =
  let hash = hash(label)
  for idx, lens in enumerate(boxes[hash]):
    if lens.label == label:
      return (hash, some(idx))
  return (hash, none(int))

let instructions = stdin.readAll.strip.split(",").toSeq

if paramStr(1) == "1":
  let res = instructions.mapIt(uint64(hash(it))).foldl(a + b)
  echo(fmt"{res}")
else:
  var boxes: Boxes
  for instr in instructions:
    var
      label: string
      strength: int
    if scanf(instr, "$w=$i", label, strength):
      let (boxIdx, lensIdx) = findLens(boxes, label)
      if lensIdx.isSome:
        boxes[boxIdx][lensIdx.get] = (label, strength)
      else:
        boxes[boxIdx].add((label, strength))
    if scanf(instr, "$w-", label):
      let (boxIdx, lensIdx) = findLens(boxes, label)
      if lensIdx.isSome:
        boxes[boxIdx].delete(lensIdx.get)

  var res = 0
  for boxIdx, box in enumerate(boxes):
    for slotIdx, lens in enumerate(box):
      res += (boxIdx + 1) * (slotIdx + 1) * lens.strength

  echo(fmt"{res}")

# vim: ts=2 sw=2 et
