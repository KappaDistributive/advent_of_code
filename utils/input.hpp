#pragma once

#include <iostream>
#include <filesystem>
#include <fstream>
#include <string>

namespace utils {
  class Reader {
  private:
    std::filesystem::path input;

  public:
    /**
     * 
     * @param input Path to text file containing the puzzle input.
     */
    Reader(std::filesystem::path input);
    
    // read input and convert it to a string
    std::string to_string();
  };
}