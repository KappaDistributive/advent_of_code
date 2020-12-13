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


  std::vector<std::string> split_string(const std::string& input, const char delimiter)
  {
    std::vector<std::string> splits;
    std::string buffer;

    for (auto character: input)
    {
      if (character == delimiter)
      {
        splits.push_back(buffer);
        buffer = "";
      }
      else
      {
        buffer += character;
      }
    }
    if (buffer.size() > 0)
    {
      splits.push_back(buffer);
    }

    return splits;
  }
}
