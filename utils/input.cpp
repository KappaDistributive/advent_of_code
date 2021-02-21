#include <cassert>

#include "input.hpp"

namespace utils {
  Reader::Reader(std::filesystem::path input) : input(input) {}


  std::vector<std::string> Reader::get_lines() {
    std::string line;
    std::vector<std::string> result;
    std::ifstream input_file(input);

    while (std::getline(input_file, line)) {
      result.push_back(line);
    }
    input_file.close();

    return result;
  }


  std::vector<std::string> split_string(const std::string& input, const char delimiter) {
    std::vector<std::string> splits;
    std::string buffer;

    for (auto character: input) {
      if (character == delimiter) {
        splits.push_back(buffer);
        buffer = "";
      } else {
        buffer += character;
      }
    }
    if (buffer.size() > 0) {
      splits.push_back(buffer);
    }

    return splits;
  }

  template<typename T>
  std::vector<T> rotate_vector(const std::vector<T>& input, const int& rotation) {
    std::vector<T> result;
    size_t size = input.size();
    result.resize(size);
    int steps;
    if (rotation>= 0) {
      steps = rotation % size;
      for (size_t index{0}; index < size; index ++) {
        result[(index + steps) % size] = input[index];
      }
    } else {
      steps = -rotation % size;
      for (size_t index{0}; index < size; index ++) {
        result[index] = input[(index + steps) % size];
      }
    }
    return result;
  }

  template std::vector<int> rotate_vector<int> (const std::vector<int>&, const int&);
  template std::vector<bool> rotate_vector<bool> (const std::vector<bool>&, const int&);

  int pow(int base, int exponent) {
    assert(exponent >= 0);
    if (exponent == 0) {
      return 1;
    } else {
      return base * pow(base, exponent-1);
    }
  }
} // namespace utils
