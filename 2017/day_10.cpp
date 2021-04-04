#include "../utils/input.hpp"
#include "../utils/tree.hpp"

std::vector<int> prepareInput(const std::vector<std::string>& input) {
  std::vector<int> result;
  for (auto entry : input) {
    result.push_back(std::stoi(entry));
  }

  return result;
}

std::vector<int> formKnot(const std::vector<int>& chain,
                          const size_t& start,
                          const size_t& knot_length) {
  assert(start < chain.size());
  assert(knot_length <= chain.size());
  auto knot = utils::rotate_vector(chain, -start);

  for (size_t index{0}; index < knot_length / 2; index++) {
    auto memory = knot[index];
    knot[index] = knot[knot_length - index - 1];
    knot[knot_length - index - 1] = memory;
  }
  return utils::rotate_vector(knot, start);
}


int part_one(const std::vector<std::string>& input) {
  auto lengths = prepareInput(input);
  std::vector<int> chain;
  for (size_t index{0}; index < 256; index++) {
    chain.push_back(index);
  }
  size_t index{0};
  size_t skip_size{0};
  for (auto length : lengths) {
    chain = formKnot(chain, index, length);
    index = (index + length + skip_size) % chain.size();
    skip_size++;
  }

  return chain[0] * chain[1];
}


int part_two(const std::vector<std::string>& input) {
  return 124;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_10.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
