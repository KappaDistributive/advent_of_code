#include "input.hpp"

namespace utils {
  Reader::Reader(std::filesystem::path input) : input(input) {}


  std::vector<std::string> Reader::get_lines() {
    std::string line;
    std::vector<std::string> result;
    std::ifstream input_file(input);

    while (std::getline (input_file, line)) {
      result.push_back(line);
    }
    input_file.close();

    return result;
  }
}
