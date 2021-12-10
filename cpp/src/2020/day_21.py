#! python3

from typing import Dict, List, Set, Tuple

def prepare_input(raw_input: List[str]) -> List[Tuple[List[str], List[str]]]:
  result = []

  for line in raw_input:
    ingredients, allergenes = line.split("(contains ");
    ingredients = ingredients.strip().split(" ")
    allergenes = allergenes.rstrip(")").split(", ")
    result.append((ingredients, allergenes))

  return result

def determine_uniques(formatted_input: List[Tuple[List[str], List[str]]], kind: str) -> List[str]:
  uniques = set()
  for ingredients, allergenes in formatted_input:
    if kind == "ingredients":
      for ingredient in ingredients:
        uniques.add(ingredient)
    elif kind == "allergenes":
      for allergene in allergenes:
        uniques.add(allergene)

  return sorted(list(uniques))

def determine_candidates(unique_ingredients: List[str], unique_allergenes: List[str]) -> Dict[str, Set[str]]:
  candidates = {}

  for allergene in unique_allergenes:
      candidates[allergene] = set(unique_ingredients)

  for ingredients, allergenes in formatted_input:
    for allergene in allergenes:
      candidates[allergene] = candidates[allergene].intersection(set(ingredients))

  return candidates

def determine_safe_ingredients(
    candidates: Dict[str, Set[str]],
    unique_ingredients: List[str],
    unique_allergenes: List[str]
) -> Set[str]:
  safe_ingredients = set()

  for ingredient in unique_ingredients:
    is_safe = True
    for allergene in unique_allergenes:
      if ingredient in candidates[allergene]:
        is_safe = False
        break
    if is_safe:
      safe_ingredients.add(ingredient)

  return safe_ingredients

def part_one(formatted_input: List[Tuple[List[str], List[str]]]) -> str:
  unique_ingredients = determine_uniques(formatted_input, "ingredients")
  unique_allergenes = determine_uniques(formatted_input, "allergenes")
  candidates = determine_candidates(unique_ingredients, unique_allergenes)
  safe_ingredients = determine_safe_ingredients(candidates, unique_ingredients, unique_allergenes)

  safe_ingredient_counter = {ingredient: 0 for ingredient in safe_ingredients}
  counter = 0

  for ingredients, _ in formatted_input:
    for ingredient in ingredients:
      if ingredient in safe_ingredient_counter.keys():
        safe_ingredient_counter[ingredient] += 1
        counter +=1

  return counter

def part_two(formatted_input: List[Tuple[List[str], List[str]]]) -> int:
  unique_ingredients = determine_uniques(formatted_input, "ingredients")
  unique_allergenes = determine_uniques(formatted_input, "allergenes")
  candidates = determine_candidates(unique_ingredients, unique_allergenes)
  safe_ingredients = determine_safe_ingredients(candidates, unique_ingredients, unique_allergenes)

  updates = True
  while updates:
    updates = False
    for allergene_index in range(len(unique_allergenes)):
      if len(candidates[unique_allergenes[allergene_index]]) == 1:
        unicorn_allergene = unique_allergenes[allergene_index]
        unicorn_ingredient = next(iter(candidates[unicorn_allergene]))
        for allergene in candidates.keys():
          if allergene != unicorn_allergene and unicorn_ingredient in candidates[allergene]:
            candidates[allergene].remove(unicorn_ingredient)
            updates = True

  result = sorted([(next(iter(candidates[allergene])), allergene) for allergene in unique_allergenes], key=lambda x: x[1])
  return ",".join([x[0] for x in result])

if __name__ == "__main__":
  raw_input = open("data/input_21.txt").read().strip().split("\n")
  formatted_input = prepare_input(raw_input)

  print("The answer to part one is:", part_one(formatted_input))
  print("The answer to part two is:", part_two(formatted_input))
