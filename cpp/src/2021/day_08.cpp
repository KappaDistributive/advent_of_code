#include <cassert>

#include "../utils/input.hpp"

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<std::array<std::string, 10>> signals;
  std::vector<std::array<std::string, 4>> outputs;
  for (auto line : input) {
    auto splits = utils::split_string(line, ' ');
    assert(splits.size() == 15);
    for (size_t index{0}; index < splits.size(); ++index) {
      std::sort(splits[index].begin(), splits[index].end());
    }
    signals.push_back(std::array<std::string, 10>{
        splits[0],
        splits[1],
        splits[2],
        splits[3],
        splits[4],
        splits[5],
        splits[6],
        splits[7],
        splits[8],
        splits[9],
    });
    outputs.push_back(std::array<std::string, 4>{
        splits[11],
        splits[12],
        splits[13],
        splits[14],
    });
  }
  return std::make_pair(signals, outputs);
}

auto part_one(const std::vector<std::string>& input) {
  size_t result{0};
  for (auto line : input) {
    auto splits = utils::split_string(line, ' ');
    for (int offset{0}; offset < 4; ++offset) {
      auto length = splits[splits.size() - 1 - offset].size();
      // std::cout << splits[splits.size() - 1 - offset] << std::endl;
      result += length == 2 || length == 3 || length == 4 || length == 7;
    }
  }
  return result;
}

auto part_two(const std::vector<std::string>& input) {
  auto [signals, outputs] = prepare_input(input);
  size_t result{0};

  assert(signals.size() == outputs.size());

  for (size_t index{0}; index < signals.size(); ++index) {
    std::array<std::string, 10> decoded;
    std::set<std::string> lhs;
    for (auto signal : signals[index]) {
      lhs.insert(signal);
    }

    for (auto signal : lhs) {
      if (signal.size() == 2) {
        decoded[1] = signal;
      }
    }
    lhs.erase(decoded[1]);

    for (auto signal : lhs) {
      if (signal.size() == 4) {
        decoded[4] = signal;
      }
    }
    lhs.erase(decoded[4]);

    for (auto signal : lhs) {
      if (signal.size() == 3) {
        decoded[7] = signal;
      }
    }
    lhs.erase(decoded[7]);

    for (auto signal : lhs) {
      if (signal.size() == 7) {
        decoded[8] = signal;
      }
    }
    lhs.erase(decoded[8]);

    for (auto signal : lhs) {
      size_t additional_chars{0};
      size_t missing_chars{0};
      std::set<char> local_chars;
      for (auto ch : decoded[4]) {
        local_chars.insert(ch);
      }
      for (auto ch : decoded[7]) {
        local_chars.insert(ch);
      }

      for (auto character : signal) {
        if (std::find(local_chars.begin(), local_chars.end(), character) ==
            local_chars.end()) {
          ++missing_chars;
        }
      }
      for (auto character : local_chars) {
        if (std::find(signal.begin(), signal.end(), character) ==
            signal.end()) {
          ++additional_chars;
        }
      }
      if (missing_chars + additional_chars == 1) {
        decoded[9] = signal;
      }
    }
    lhs.erase(decoded[9]);

    for (auto signal : lhs) {
      if (signal.size() != 6) {
        continue;
      }
      bool is_zero{true};
      for (auto character : decoded[1]) {
        if (std::find(signal.begin(), signal.end(), character) ==
            signal.end()) {
          is_zero = false;
          break;
        }
      }

      if (is_zero) {
        decoded[0] = signal;
      }
    }
    lhs.erase(decoded[0]);

    for (auto signal : lhs) {
      if (signal.size() == 6) {
        decoded[6] = signal;
        break;
      }
    }
    lhs.erase(decoded[6]);

    for (auto signal : lhs) {
      if (signal.size() != 5) {
        continue;
      }
      bool is_three{true};
      for (auto character : decoded[1]) {
        if (std::find(signal.begin(), signal.end(), character) ==
            signal.end()) {
          is_three = false;
          break;
        }
      }

      if (is_three) {
        decoded[3] = signal;
      }
    }
    lhs.erase(decoded[3]);

    for (auto signal : lhs) {
      size_t missing_characters{0};
      for (auto character : decoded[9]) {
        if (std::find(signal.begin(), signal.end(), character) ==
            signal.end()) {
          ++missing_characters;
        }
      }
      if (missing_characters == 1) {
        decoded[5] = signal;
        break;
      }
    }
    lhs.erase(decoded[5]);

    decoded[2] = *lhs.begin();

    size_t minor_result{0};
    size_t power{1000};
    for (auto output : outputs[index]) {
      for (size_t digit{0}; digit < 10; ++digit) {
        if (decoded[digit] == output) {
          minor_result += power * digit;
          power /= 10;
        }
      }
    }
    result += minor_result;
    std::cout << minor_result << std::endl;
  }

  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2021/input_08_mock.txt"};
  std::filesystem::path input_path{"../../data/2021/input_08.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
