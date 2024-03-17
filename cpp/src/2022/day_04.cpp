#include <cassert>

#include "../utils/input.hpp"

struct Section {
  size_t start;
  size_t end;

  Section(const std::string &input) {
    auto splits = utils::split_string(input, '-');
    assert(splits.size() == 2);
    start = std::stoul(splits[0]);
    end = std::stoul(splits[1]);
  }

  bool contains(const Section &other) const {
    return other.start >= this->start && other.end <= this->end;
  }

  bool overlaps(const Section &other) const {
    return !(other.end < this->start || other.start > this->end);
  }
};

auto prepare_input(const std::vector<std::string> &input) {
  std::vector<std::pair<Section, Section>> result;
  for (const auto &line : input) {
    auto splits = utils::split_string(line, ',');
    assert(splits.size() == 2);
    result.push_back({Section(splits[0]), Section(splits[1])});
  }

  return result;
}

auto part_one(const std::vector<std::pair<Section, Section>> &input) {
  size_t answer{0};
  for (const auto &[lhs, rhs] : input) {
    if (lhs.contains(rhs) || rhs.contains(lhs)) {
      ++answer;
    }
  }
  return answer;
}

auto part_two(const std::vector<std::pair<Section, Section>> &input) {
  size_t answer{0};
  for (const auto &[lhs, rhs] : input) {
    if (lhs.overlaps(rhs)) {
      ++answer;
    }
  }
  return answer;
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_04_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_04.txt"};
  utils::Reader reader(input_path);
  auto input = prepare_input(reader.get_lines());

  std::cout << std::format("The answer to part one is: {}", part_one(input)) << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(input)) << std::endl;

  return 0;
}
