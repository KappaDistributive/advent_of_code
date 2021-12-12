#include <cassert>
#include <list>

#include "../utils/input.hpp"

void invent(size_t* jolly,
            size_t* frosty,
            std::vector<uint8_t>* scores,
            std::list<uint8_t>*  last_twenty) {
  uint8_t new_recipe = scores->operator[](*jolly) +
                       scores->operator[](*frosty);
  if (new_recipe >= 10) {
    scores->push_back(new_recipe / 10);
    last_twenty->push_back(new_recipe / 10);
  }
  scores->push_back(new_recipe % 10);
  last_twenty->push_back(new_recipe % 10);
  while (last_twenty->size() > 20) {
    last_twenty->pop_front();
  }

  *jolly =  (*jolly +
             static_cast<size_t>(scores->operator[](*jolly)) +
             1) % scores->size();
  *frosty = (*frosty +
             scores->operator[](*frosty) +
             1) % scores->size();
}

void print(size_t jolly,
           size_t frosty,
           const std::vector<uint8_t>& scores,
           const std::list<uint8_t> last_twenty = {}) {
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
  if (last_twenty.size() > 0) {
    for (auto i : last_twenty)
      std::cout << static_cast<int>(i);
    std::cout << std::endl;
  }
}

std::tuple<size_t, size_t, std::vector<uint8_t>, std::list<uint8_t>>
init() {
  return {0, 1, {3, 7}, {3, 7}};
}

bool terminate(const std::string& input,
               const std::list<uint8_t>& last_twenty) {
  std::string pattern;
  for (auto p : last_twenty)
    pattern += std::to_string(static_cast<int>(p));

  return pattern.find(input) != std::string::npos;
}

size_t part_one(const std::string& input) {
  bool verbose{false};
  size_t offset = std::stoul(input);

  auto [jolly, frosty, scores, last_twenty] = init();
  if (verbose)
    print(jolly, frosty, scores, last_twenty);
  while (scores.size() < offset + 10) {
    invent(&jolly, &frosty, &scores, &last_twenty);
    if (verbose)
      print(jolly, frosty, scores, last_twenty);
  }
  size_t result{0};

  for (size_t index{0}; index < 10; index++) {
    result *= 10;
    result += scores[offset + index];
  }

  return result;
}


size_t part_two(const std::string& input) {
  bool verbose{false};
  size_t offset = std::stoul(input);
  assertm(input == std::to_string(offset), "Encountered an unexpected input length.");

  auto [jolly, frosty, scores, last_twenty] = init();
  if (verbose)
    print(jolly, frosty, scores, last_twenty);
  while (true) {
    invent(&jolly, &frosty, &scores, &last_twenty);
    if (verbose)
      print(jolly, frosty, scores, last_twenty);

    if (terminate(input, last_twenty))
      break;
  }
  std::string pattern;
  for (auto score : scores) {
    pattern += std::to_string(static_cast<int>(score));
  }

  return pattern.find(input);
}


int main() {
  utils::Reader reader(std::filesystem::path("../../data/2018/input_14.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
