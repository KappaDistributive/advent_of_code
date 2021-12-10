#include "../utils/input.hpp"

using ingredient = std::array<int, 5>;

std::ostream& operator<< (std::ostream& os, ingredient ingredient) {
  os << "capacity: " << ingredient[0]
     << " durability: " << ingredient[1]
     << " flavor: " << ingredient[2]
     << " texture: " << ingredient[3]
     << " calories: " << ingredient[4];
  return os;
}

std::vector<ingredient> get_ingredients(const std::vector<std::string>& input) {
  std::regex re{"^.*\\s(-?\\d+).*\\s(-?\\d+).*\\s(-?\\d+).*\\s(-?\\d+).*\\s(-?\\d+).*$"};
  std::smatch matches;
  std::vector<std::array<int, 5>> ingredients;

  for (auto line: input) {
    std::regex_match(line, matches, re);
    assert(matches.size() == 6);
    ingredients.push_back(
        {
          std::stoi(matches[1].str()),
          std::stoi(matches[2].str()),
          std::stoi(matches[3].str()),
          std::stoi(matches[4].str()),
          std::stoi(matches[5].str())
       }
    );
  }

  return ingredients;
}

int part_one(const std::vector<std::string>& input) {
  int result{0}, first{0}, capacity, durability, flavor, texture;

  auto ingredients = get_ingredients(input);
  assert(ingredients.size() == 4);

  for (int fourth{0}; fourth <= 100; fourth++) {
    for (int third{0}; third + fourth <= 100; third++) {
      for (int second{0}; second + third + fourth <= 100; second++) {
        first = 100 - second - third - fourth;
        capacity = first * ingredients[0][0] + second * ingredients[1][0] + third * ingredients[2][0] + fourth * ingredients[3][0];
        capacity *= (capacity > 0);
        durability = first * ingredients[0][1] + second * ingredients[1][1] + third * ingredients[2][1] + fourth * ingredients[3][1];
        durability *= (durability > 0);
        flavor = first * ingredients[0][2] + second * ingredients[1][2] + third * ingredients[2][2] + fourth * ingredients[3][2];
        flavor *= (flavor > 0);
        texture = first * ingredients[0][3] + second * ingredients[1][3] + third * ingredients[2][3] + fourth * ingredients[3][3];
        texture *= (texture > 0);

        if (capacity * durability * flavor * texture > result) {
          result = capacity * durability * flavor * texture;
        }
      }
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input) {
  int result{0}, first{0}, capacity, durability, flavor, texture, calories;

  auto ingredients = get_ingredients(input);
  assert(ingredients.size() == 4);

  for (int fourth{0}; fourth <= 100; fourth++) {
    for (int third{0}; third + fourth <= 100; third++) {
      for (int second{0}; second + third + fourth <= 100; second++) {
        first = 100 - second - third - fourth;

        calories = first * ingredients[0][4] + second * ingredients[1][4] + third * ingredients[2][4] + fourth * ingredients[3][4];
        if (calories != 500) {
          continue;
        }
        capacity = first * ingredients[0][0] + second * ingredients[1][0] + third * ingredients[2][0] + fourth * ingredients[3][0];
        capacity *= (capacity > 0);
        durability = first * ingredients[0][1] + second * ingredients[1][1] + third * ingredients[2][1] + fourth * ingredients[3][1];
        durability *= (durability > 0);
        flavor = first * ingredients[0][2] + second * ingredients[1][2] + third * ingredients[2][2] + fourth * ingredients[3][2];
        flavor *= (flavor > 0);
        texture = first * ingredients[0][3] + second * ingredients[1][3] + third * ingredients[2][3] + fourth * ingredients[3][3];
        texture *= (texture > 0);

        if (capacity * durability * flavor * texture > result) {
          result = capacity * durability * flavor * texture;
        }
      }
    }
  }
  return result;
  }

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2015/input_15.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

