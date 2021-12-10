#include "../utils/input.hpp"


std::vector<int> prepare_input(const std::vector<std::string>& input) {
  std::vector<int> jumps;
  for (auto line : input) {
    jumps.push_back(std::stoi(line));
  }
  return jumps;
}


int64_t part_one(const std::vector<std::string>& input) {
  auto jumps = prepare_input(input);
  size_t position{0};
  int64_t step{0};
  do {
    position += jumps[position]++;
    step++;
  } while (position < jumps.size());
  return step;
}


int part_two(const std::vector<std::string>& input) {
  auto jumps = prepare_input(input);
  size_t position{0};
  int64_t step{0};
  int jump_value;
  do {
    jump_value = jumps[position];
    if (jump_value >= 3) {
      jumps[position]--;
    } else {
      jumps[position]++;
    }
    position += jump_value;
    step++;
  } while (position < jumps.size());
  return step;
}


int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_05.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
