#include <cassert>
#include <deque>
#include <list>
#include <vector>

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


std::vector<std::string>
split_string(const std::string& input, const char& delimiter) {
  std::vector<std::string> splits;
  std::string buffer;

  for (auto character : input) {
    if (character == delimiter) {
      if (buffer.size() > 0) {
        splits.push_back(buffer);
      }
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

void replace_all_substrings(std::string* input,
                            const std::string& search,
                            const std::string& replacement) {
  while (input->find(search) != std::string::npos) {
      input->replace(input->find(search), search.size(), replacement);
  }
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

template std::vector<bool>
rotate_vector<bool>(const std::vector<bool>&, const int&);
template std::vector<char>
rotate_vector<char>(const std::vector<char>&, const int&);
template std::vector<int>
rotate_vector<int>(const std::vector<int>&, const int&);

template<typename T>
T pow(T base, T exponent) {
  assert(exponent >= 0);
  if (exponent == 0) {
    return 1;
  } else {
    return base * pow(base, exponent-1);
  }
}

template int pow<int>(int, int);
template size_t pow<size_t>(size_t, size_t);
template int64_t pow<int64_t>(int64_t, int64_t);

template<typename T>
T factorial(T n) {
  if (n <= 1) {
    return 1;
  } else {
    return n * factorial(n-1);
  }
}

template int factorial<int>(int);
template int64_t factorial<int64_t>(int64_t);
template size_t factorial<size_t>(size_t);


template<typename T>
std::string stringify(T input) {
  std::string result;
  for (auto c : input) {
    result += c;
  }

  return result;
}

template std::string stringify(std::vector<char>);
template std::string stringify(std::deque<char>);

}  // namespace utils
