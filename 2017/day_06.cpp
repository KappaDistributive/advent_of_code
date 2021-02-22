#include <algorithm>

#include "../utils/input.hpp"

std::vector<int> prepare_input(const std::vector<std::string>& input) {
  std::vector<int> blocks;
  for (auto line: input) {
    blocks.push_back(std::stoi(line));
  }
  return blocks;
}

std::vector<std::vector<int>> get_loop(const std::vector<int> initial_state) {
  auto state = initial_state;
  std::vector<std::vector<int>> states{{initial_state}};
  size_t index_max;
  int max;

  while (true) {
    max = state[0];
    index_max = 0;
    for (size_t index{0}; index < state.size(); index++) {
      if (state[index] > max) {
        index_max = index;
        max = state[index_max];
      }
    }
    state[index_max] = 0;
    while (max > 0) {
      index_max = (index_max + 1) % state.size();
      state[index_max]++;
      max--;
    }
    if (std::find(states.begin(), states.end(), state) != states.end()) {
      states.push_back(state);
      break;
    } else {
      states.push_back(state);
    }
  }
  return states;
}

int part_one(const std::vector<std::string>& input) {
  auto blocks = prepare_input(input);
  auto loop = get_loop(blocks);
  return loop.size() - 1;
}

int part_two(const std::vector<std::string>& input) {
  auto blocks = get_loop(prepare_input(input)).back();
  auto loop = get_loop(blocks);
  return loop.size() - 1;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_06.txt"));
  auto input = utils::split_string(reader.get_lines()[0], '\t');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

