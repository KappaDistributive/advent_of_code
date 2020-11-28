#include "input.hpp"

namespace utils {
  Reader::Reader(std::filesystem::path input) : input(input) {}


  std::string Reader::to_string() {
    std::string line;
    std::string result;
    std::ifstream input_file(input);

    while (std::getline (input_file, line)) {
      result += line;
    }
    input_file.close();

    return result;
  }
}
