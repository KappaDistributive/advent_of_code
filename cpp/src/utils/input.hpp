#pragma once

#include <algorithm>
#include <array>
#include <cassert>
#include <filesystem>
#include <format>
#include <fstream>
#include <functional>
#include <iomanip>
#include <iostream>
#include <list>
#include <map>
#include <numeric>
#include <optional>
#include <regex> // NOLINT
#include <set>
#include <sstream>
#include <stdexcept>
#include <string>
#include <unordered_set>
#include <vector>

inline int _assert_message(const char *condition, const char *function,
                           const char *file, int line, const char *message) {
  std::cerr << std::format(
                   "Assertion failed ({}),\nfunction {}, location {}:{}\n{}",
                   condition, function, file, line, message)
            << std::endl;
  std::abort();
  return 0;
}

#ifdef NDEBUG
#define assertm(condition, message) 0
#else
#define assertm(condition, message)                                            \
  (!(static_cast<bool>(condition)))                                            \
      ? _assert_message(#condition, __FUNCTION__, __FILE__, __LINE__, message) \
      : 1
#endif

namespace utils {
class Reader {
private:
  std::filesystem::path input;

public:
  /**
   *
   * @param input Path to text file containing the puzzle input.
   */
  explicit Reader(std::filesystem::path input);

  // read input and convert it to a vector of strings
  std::vector<std::string> get_lines();
};

std::vector<std::string> split_string(const std::string &input,
                                      const char &delimiter);

std::vector<std::string> split_string(const std::string &input,
                               const std::string &delimiter);

void replace_all_substrings(std::string *input, const std::string &search,
                            const std::string &replacement);

template <typename T>
std::vector<T> rotate_vector(const std::vector<T> &input, const int &rotation);

template <typename T> T pow(T base, T exponent);

template <typename T> T factorial(T n);

template <typename T> std::string stringify(T input);

// Returns 1 if x > 0, -1 if x < 0, and 0 if x is zero.
template <class T> int sign(const T &z);

} // namespace utils
