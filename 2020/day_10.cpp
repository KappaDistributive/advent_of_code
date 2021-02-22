#include <algorithm>
#include <cassert>

#include "../utils/input.hpp"

std::vector<int> get_jolts(const std::vector<std::string>& input) {
  std::vector<int> jolts;
  int value{0}, max{0};
  jolts.push_back(0);
  for (auto jolt: input) {
    value = std::stoi(jolt);
    if (value > max) {
      max = value;
    }
    jolts.push_back(value);
  }
  jolts.push_back(max+3);

  return jolts;
}

bool indicator(int jolt, const std::vector<int>& jolts) {
  for (auto j: jolts) {
    if (j == jolt) {
      return true;
    } else if (j > jolt) {
      break;
    }
  }
  return false;
}

int part_one(const std::vector<std::string>& input) {
  int result{0};
  int distribution[] = {0, 0, 0};
  int diff{0};
  auto jolts = get_jolts(input);

  std::sort(jolts.begin(), jolts.end());

  for (size_t index{1}; index < jolts.size(); index++) {
    diff = std::abs(jolts[index-1] - jolts[index]);
    assert(1 <= diff && diff <= 3);
    distribution[diff - 1]++;
  }

  return distribution[0] * distribution[2];
}

int64_t part_two(const std::vector<std::string>& input) {
  auto jolts = get_jolts(input);
  std::sort(jolts.begin(), jolts.end());

  std::vector<int64_t> paths;
  int64_t max = jolts[jolts.size() - 1];
  paths.reserve(max +  1);
  paths[0] = 1;
  paths[1] = static_cast<int64_t>(indicator(1, jolts));
  paths[2] = static_cast<int64_t>(indicator(2, jolts) * (paths[0] + paths[1]));

  for (size_t index{3}; index < max + 1; index++) {
    paths[index] = static_cast<int64_t>(indicator(index, jolts) * (paths[index-3] + paths[index-2] + paths[index-1]));
  }
  return paths[max];
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_10.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

