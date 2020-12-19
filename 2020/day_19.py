#! python3
from typing import List, Tuple

import nltk
from tqdm import tqdm

def prepare_rules(rules: List[str], part_two: bool = False) -> str:
  for index in range(len(rules)):
    if part_two:
      if rules[index].startswith("8:"):
        rules[index] = "8 -> 42 | 42 8"
        continue
      elif rules[index].startswith("11:"):
        rules[index] = "11 -> 42 31 | 42 11 31"
        continue
    rules[index] = rules[index].replace(":", " ->")

  return "\n".join(sorted(rules))

def prepare_input(raw_input: List[str]) -> Tuple[List[str], List[str]]:
  rules: List[str] = []
  data: List[str] = []
  section_one: bool = True

  for line in raw_input:
    if line.rstrip("\n") == "":
      section_one = False
      continue
    if section_one:
      rules.append(line.rstrip("\n"))
    else:
      data.append(line.rstrip("\n"))

  return (rules, data)

def part_one(raw_input: List[str]) -> int:
  rules, data = prepare_input(raw_input)
  result: int = 0
  grammar = nltk.CFG.fromstring(prepare_rules(rules))
  print(grammar)
  parser = nltk.ChartParser(grammar)

  for entry in tqdm(data):
    try:
      if len(list(parser.parse(entry))) > 0:
        result += 1
    except ValueError as err:
      print(entry)
      print(err)

  return result

def part_two(raw_input: List[str]) -> int:
  rules, data = prepare_input(raw_input,)
  result: int = 0
  grammar = nltk.CFG.fromstring(prepare_rules(rules, True))
  print(grammar)
  parser = nltk.ChartParser(grammar)

  for entry in tqdm(data):
    try:
      if len(list(parser.parse(entry))) > 0:
        result += 1
    except ValueError as err:
      print(entry)
      print(err)

  return result

if __name__ == "__main__":
  with open("data/input_19.txt", "r") as f:
    raw_input = f.readlines()

  print(f"The answer to part one is: {part_one(raw_input)}")
  print(f"The answer to part two is: {part_two(raw_input)}")
