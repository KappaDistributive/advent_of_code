#include "../utils/input.hpp"

std::string part_one(const std::vector<std::string>& input) {
  std::vector<char> programs = {'a', 'b', 'c', 'd',
                                'e', 'f', 'g', 'h',
                                'i', 'j', 'k', 'l',
                                'm', 'n', 'o', 'p'};
  for (auto instruction : input) {
    auto tail = instruction.substr(1);
    char op = instruction[0];
    if (op == 's') {
      programs = utils::rotate_vector(programs, std::stoi(tail));
    } else if (op == 'x') {
      auto splits = utils::split_string(tail, '/');
      assert(splits.size() == 2);
      auto memory = programs[std::stoi(splits[0])];
      programs[std::stoi(splits[0])] = programs[std::stoi(splits[1])];
      programs[std::stoi(splits[1])] =  memory;
    } else if (op == 'p') {
      auto splits = utils::split_string(tail, '/');
      assert(splits.size() == 2);
      assert(splits[0].size() == 1);
      assert(splits[1].size() == 1);
      auto left = splits[0][0];
      auto right = splits[1][0];
      auto left_index = std::distance(
        programs.begin(),
        std::find(programs.begin(), programs.end(), left));
      auto right_index = std::distance(
        programs.begin(),
        std::find(programs.begin(), programs.end(), right));
      auto memory = programs[left_index];
      programs[left_index] = programs[right_index];
      programs[right_index] = memory;
    } else {
      throw std::runtime_error("Unknown instruction: " + instruction);
    }
  }
  std::string result;
  for (auto p : programs) {
    result.push_back(p);
  }

  return result;
}


int part_two(const std::vector<std::string>& input) {
  return 2;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_16.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
