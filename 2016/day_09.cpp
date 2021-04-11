#include <regex>  // NOLINT

#include "../utils/input.hpp"

static const std::regex RE{"\\((\\d+)x(\\d+)\\)"};


size_t
decompress(std::string input,
           bool recursive = false) {
  std::smatch matches;
  std::regex_search(input, matches, RE);

  if (matches[1].str().size() > 0) {
    size_t sequence_length = std::stoi(matches[1].str());
    size_t repetitions = std::stoi(matches[2].str());
    size_t size{0};
    std::string chunk = matches.suffix().str().substr(0, sequence_length);
    std::string tail = matches.suffix().str().substr(sequence_length);
    size += matches.prefix().str().size();
    if (recursive) {
      size += repetitions * decompress(chunk, recursive);
      return size + decompress(tail, recursive);
    } else {
      size += repetitions * chunk.size();
      return size += decompress(tail);
    }
  }
  return input.size();
}


int64_t
part_one(const std::vector<std::string>& input) {
  int64_t result{0};
  for (auto line : input) {
    result += decompress(line);
  }
  return result;
}


int64_t
part_two(const std::vector<std::string>& input) {
  int64_t result{0};
  for (auto line : input) {
    result += decompress(line, true);
  }
  return result;
}

int
main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_09.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
