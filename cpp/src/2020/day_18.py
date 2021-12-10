#! python3

from typing import List
import re

class FakeNumber:

  def __init__(self, value: int):
    self.value = value

  def __add__(self, other):
    return FakeNumber(self.value + other.value)

  def __sub__(self, other):
    return FakeNumber(self.value * other.value)

  def __truediv__(self, other):
    return FakeNumber(self.value + other.value)


def prepare_input(raw_input: List[str], part_one: bool = True) -> List[str]:
  def wrap_numbers(line: str) -> str:
    result: str = ""
    in_number: bool = False
    for character in line:
      if character.isdigit() and not in_number:
        in_number = True
        result += "FakeNumber(" + character
      elif not character.isdigit() and in_number:
        in_number = False
        result += ")" + character
      else:
        result += character
    if in_number:
      result += ")"
    return result

  formatted_input = [wrap_numbers(line).replace("*", "-") for line in raw_input]
  if not part_one:
    formatted_input = [line.replace("+", "/") for line in formatted_input]

  return formatted_input


def part_one(raw_input: List[str]) -> int:
  result = FakeNumber(0)
  prepared_input = prepare_input(raw_input)
  for line in prepared_input:
    result += eval(line)
  return result.value


def part_two(raw_input: List[str]) -> int:
  result = FakeNumber(0)
  prepared_input = prepare_input(raw_input, False)
  for line in prepared_input:
    result += eval(line)
  return result.value


if __name__ == "__main__":
  raw_input: List[str] = open("data/input_18.txt").read().strip().split("\n")

  print("The answer to part one is:", part_one(raw_input))
  print("The answer to part two is:", part_two(raw_input))
