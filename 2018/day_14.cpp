#include "../utils/input.hpp"

void invent(size_t* jolly, size_t* frosty, std::vector<uint8_t>* scores) {
  uint8_t new_recipe = scores->operator[](*jolly) +
                       scores->operator[](*frosty);
  if (new_recipe >= 10) {
    scores->push_back(new_recipe / 10);
  }
  scores->push_back(new_recipe % 10);

  *jolly =  (*jolly +
             static_cast<size_t>(scores->operator[](*jolly)) +
             1) % scores->size();
  *frosty = (*frosty +
             scores->operator[](*frosty) +
             1) % scores->size();
}

void print(size_t jolly, size_t frosty, const std::vector<uint8_t>& scores) {
  for (size_t index{0}; index < scores.size(); index++) {
    char prefix{' '}, suffix{' '};
    if (index == jolly) {
      prefix = '(';
      suffix = ')';
    } else if (index == frosty) {
      prefix = '[';
      suffix = ']';
    }
    std::cout << prefix << static_cast<int>(scores[index]) << suffix;
  }
  std::cout << std::endl;
}

std::tuple<size_t, size_t, std::vector<uint8_t>> init() {
  return {0, 1, {3, 7}};
}

size_t part_one(const std::string& input) {
  bool verbose{false};
  size_t offset = std::stoul(input);

  auto [jolly, frosty, scores] = init();
  if (verbose)
    print(jolly, frosty, scores);
  while (scores.size() < offset + 10) {
    invent(&jolly, &frosty, &scores);
    if (verbose)
      print(jolly, frosty, scores);
  }
  size_t result{0};

  for (size_t index{0}; index < 10; index++) {
    result *= 10;
    result += scores[offset + index];
  }

  return result;
}


int part_two(const std::string& input) {
  return -747;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_14.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
