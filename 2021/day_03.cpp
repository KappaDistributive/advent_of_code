#include <algorithm>

#include "../utils/input.hpp"

std::pair<size_t, size_t> bit_count(const std::vector<std::string>& candidates,
                                    size_t bit) {
  size_t ones{0};
  for (auto code : candidates) {
    ones += code[bit] == '1';
  }

  return std::make_pair(candidates.size() - ones, ones);
}

int decode(const std::vector<std::string>& input, bool gamma) {
  const auto num_bits = input[0].size();
  std::vector<size_t> ones(num_bits, 0);

  for (auto code : input) {
    for (size_t power{0}; power < num_bits; ++power) {
      ones[power] += code[num_bits - power - 1] == '1';
    }
  }

  int result{0};
  for (size_t power{0}; power < num_bits; ++power) {
    if (gamma) {
      if (2 * ones[power] > input.size()) {
        result += utils::pow(2, static_cast<int>(power));
      }
    } else {
      if (2 * ones[power] < input.size()) {
        result += utils::pow(2, static_cast<int>(power));
      }
    }
  }

  return result;
}

int decode_binary(const std::string& code) {
  int result{0};
  int value{1};
  for (size_t power{0}; power < code.size(); ++power) {
    if (code[code.size() - power - 1] == '1') {
      result += value;
    }
    value *= 2;
  }

  return result;
}

auto cull(const std::vector<std::string>& candidates, size_t bit, bool oxygen) {
  auto [zeroes, ones] = bit_count(candidates, bit);

  char filter;
  if (oxygen) {
    filter = zeroes > ones ? '0' : '1';
  } else {
    filter = zeroes > ones ? '1' : '0';
  }

  std::vector<std::string> result;
  std::copy_if(candidates.begin(), candidates.end(), std::back_inserter(result),
               [bit, filter](auto code) { return code[bit] == filter; });

  return result;
}

auto part_one(const std::vector<std::string>& input) {
  auto gamma = decode(input, true);
  auto epsilon = decode(input, false);
  std::cout << "gamma: " << gamma << "\nepsilon: " << epsilon << std::endl;

  return gamma * epsilon;
}

auto part_two(const std::vector<std::string>& input) {
  const size_t num_bits = input[0].size();
  auto candidates = input;

  for (size_t power{0}; power < num_bits; ++power) {
    candidates = cull(candidates, power, true);
    if (candidates.size() == 1) {
      break;
    }
  }
  assert(candidates.size() == 1);
  auto generator = decode_binary(candidates[0]);

  candidates = input;
  for (size_t power{0}; power < num_bits; ++power) {
    candidates = cull(candidates, power, false);
    if (candidates.size() == 1) {
      break;
    }
  }
  assert(candidates.size() == 1);
  auto scrubber = decode_binary(candidates[0]);

  std::cout << "oxygen generator rating: " << generator
            << "\nCO2 scrubber rating: " << scrubber << std::endl;

  return generator * scrubber;
}

int main() {
  // std::filesystem::path input_path{"../2021/data/input_03_mock.txt"};
  std::filesystem::path input_path{"../2021/data/input_03.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;

  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
