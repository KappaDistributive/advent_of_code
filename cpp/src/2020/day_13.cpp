#include <map>

#include "../utils/input.hpp"


std::pair<int64_t, std::vector<std::string>>
prepare_input(const std::vector<std::string>& input) {
  int64_t offset = std::stoi(input[0]);
  std::vector<std::string> ids = utils::split_string(input[1], ',');
  return std::make_pair(offset, ids);
}


int64_t next_departure(int64_t id, int64_t offset) {
  return (offset % id > 0) * (id - (offset % id)) + offset;
}


int64_t part_one(const std::vector<std::string>& input) {
  int64_t id{-1}, waiting{-1}, id_temp;
  auto [offset, ids] = prepare_input(input);

  for (auto id_string : ids) {
    try {
      id_temp = std::stoi(id_string);
      if (waiting == -1 || next_departure(id_temp, offset) - offset < waiting) {
        id = id_temp;
        waiting = next_departure(id, offset) - offset;
      }
    }
    catch (const std::invalid_argument& e) {
      continue;
    }
  }
  return id * waiting;
}


int64_t part_two(const std::vector<std::string>& input) {
  auto [_, ids] = prepare_input(input);
  std::map<int, int> factors;
  std::vector<int> offsets;
  for (size_t index{0}; index < ids.size(); index++) {
    try {
      factors.insert(std::make_pair(index, std::stoi(ids[index])));
      offsets.push_back(index);
    }
    catch (const std::invalid_argument& e) {
      continue;
    }
  }

  int64_t offset{0}, step{1};

  for (auto [index, factor] : factors) {
    while ((offset + index) % factor != 0) {
      offset += step;
    }
    step *= factor;
  }

  return offset;
}


int main() {
  utils::Reader reader(std::filesystem::path("../../data/2020/input_13.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
