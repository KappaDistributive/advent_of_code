#include "../../includes/md5.hpp"
#include "../utils/input.hpp"

std::optional<char> first_multiple(const std::string& input,
                                   const size_t multiplier) {
  for (size_t index{0}; index + multiplier <= input.size(); index++) {
    char candidate = input[index];
    bool found{true};
    for (size_t count{1}; count < multiplier; count++) {
      if (input[index + count] != candidate) {
        found = false;
        break;
      }
    }
    if (found) {
      return candidate;
    }
  }
  return std::nullopt;
}

size_t part_one(const std::string& input) {
  size_t num_keys{0};
  size_t offset{0};
  std::map<std::pair<size_t, std::string>, int> candidates;
  std::string candidate;
  std::vector<size_t> keys;
  bool final_phase{false};
  size_t final_count{0};

  while (!final_phase || final_count <= 1000) {
    candidate = md5(input + std::to_string(offset));
    for (auto [key, value] : candidates) {
      if (value == -1 && key.first + 1000 >= offset &&
          candidate.find(key.second) != std::string::npos) {
        candidates.insert_or_assign(key, offset);
        keys.push_back(key.first);
        if (num_keys == 63) {
          final_phase = true;
        }
        num_keys++;
      }
    }
    auto triple = first_multiple(candidate, 3);
    if (triple.has_value()) {
      std::string pentuple;
      for (size_t count{0}; count < 5; count++) {
        pentuple += triple.value();
      }
      candidates.insert_or_assign({offset, pentuple}, -1);
    }
    offset++;
    if (final_phase) {
      final_count++;
    }
  }
  std::sort(keys.begin(), keys.end());
  return keys[63];
}

int part_two(const std::string& input) {
  size_t num_keys{0};
  size_t offset{0};
  std::map<std::pair<size_t, std::string>, int> candidates;
  std::string candidate;
  std::vector<size_t> keys;
  bool final_phase{false};
  size_t final_count{0};

  while (!final_phase || final_count <= 1000) {
    candidate = input + std::to_string(offset);
    for (size_t hash_counter{0}; hash_counter < 2017; hash_counter++) {
      candidate = md5(candidate);
    }
    for (auto [key, value] : candidates) {
      if (value == -1 && key.first + 1000 >= offset &&
          candidate.find(key.second) != std::string::npos) {
        candidates.insert_or_assign(key, offset);
        keys.push_back(key.first);
        if (num_keys == 63) {
          final_phase = true;
        }
        num_keys++;
      }
    }
    auto triple = first_multiple(candidate, 3);
    if (triple.has_value()) {
      std::string pentuple;
      for (size_t count{0}; count < 5; count++) {
        pentuple += triple.value();
      }
      candidates.insert_or_assign({offset, pentuple}, -1);
    }
    offset++;
    if (final_phase) {
      final_count++;
    }
  }
  std::sort(keys.begin(), keys.end());
  return keys[63];
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2016/input_14.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

