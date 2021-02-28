#pragma once

#include <iostream>
#include <filesystem>
#include <fstream>
#include <string>
#include <vector>

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

std::vector<std::string> split_string(const std::string& input, const char delimiter);

void replace_all_substrings(std::string& input, const std::string& search, const std::string& replacement);


template<typename T>
std::vector<T> rotate_vector(const std::vector<T>& input, const int& rotation);

int pow(int base, int exponent);

} // namespace utils
