#include <cassert>

#include "../utils/input.hpp"

std::vector<int64_t> prepare_input(const std::vector<std::string>& input) {
  std::vector<int64_t> result;

  for (auto line: input) {
    result.push_back(std::stol(line));
  }

  return result;
}

bool valid(const std::vector<int64_t>& codes, size_t index, size_t range) {
  for (size_t index_left{0}; index_left < range; index_left++) {
    for (size_t index_right{index_left+1}; index_right < range; index_right++) {
      if
      (
        codes[index - range + index_left] != codes[index - range + index_right] &&
        codes[index - range + index_left] + codes[index - range + index_right] == codes[index]
      ) {
        return true;
      }
    }
  }

  return false;
}

int64_t part_one(const std::vector<std::string>& input) {
  int64_t result{0};
  auto codes = prepare_input(input);

  for (size_t index{25}; index< codes.size(); index++) {
    if (!valid(codes, index, 25)) {
      result = codes[index];
      break;
    }
  }
  return result;
}

int64_t part_two(const std::vector<std::string>& input) {
  auto codes = prepare_input(input);
  auto invalid_number = part_one(input);
  size_t start{0}, end{0};
  bool done = false;

  for (size_t index_left{0}; index_left < codes.size() && !done; index_left++) {
    int64_t candidate = {0};
    for (size_t index_right{index_left}; index_right < codes.size(); index_right++) {
      candidate += codes[index_right];
      if (candidate > invalid_number) {
        break;
      } else if (candidate == invalid_number) {
        done = true;
        start = index_left;
        end = index_right;
        break;
      }
    }
  }

  assert(done);
  int64_t min{-1}, max{-1};
  for (size_t index{start}; index <= end; index++) {
    if (min == -1 || codes[index] < min) {
      min = codes[index];
    }
    if (max == -1 || codes[index] > max) {

      max = codes[index];
    }
  }

  return min + max;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2020/input_09.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

