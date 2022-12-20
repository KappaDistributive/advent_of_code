#include "../utils/input.hpp"

struct Numbers {
  std::list<std::pair<int, size_t>> data;

  Numbers(const std::vector<std::string> &input) {
    for (const auto &line : input) {
      this->data.push_back(std::make_pair(std::stoi(line), this->data.size()));
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Numbers &numbers) {
    for (auto it{numbers.data.cbegin()}; it != numbers.data.cend(); ++it) {
      os << std::get<0>(*it);
      if (std::next(it) != numbers.data.cend()) {
        os << ", ";
      }
    }
    return os;
  }

  void step(size_t index) {
    for (auto it{this->data.begin()}; it != this->data.end(); ++it) {
      if (std::get<1>(*it) == index) {
        int value = std::get<0>(*it);
        int steps_taken{0};
        if (value >= 0) {
          it = this->data.erase(it);
          while (steps_taken++ < value) {
            it = std::next(it);
            if (it == this->data.end()) {
              it = this->data.begin();
            }
          }
          this->data.insert(it, std::make_pair(value, index));
          break;
        } else {
          it = this->data.erase(it);
          while (steps_taken++ < -value) {
            it = std::prev(it);
            if (it == this->data.begin()) {
              it = this->data.end();
            }
          }
          this->data.insert(it, std::make_pair(value, index));
          break;
        }
      }
    }
  }
};

auto part_one(const std::vector<std::string> &input) {
  Numbers numbers{input};
  for (size_t step{0}; step < numbers.data.size(); ++step) {
    numbers.step(step);
  }

  auto it{numbers.data.begin()};
  while (std::get<0>(*it) != 0) {
    it = std::next(it);
    if (it == numbers.data.end()) {
      it = numbers.data.begin();
    }
  }
  int result{0};
  size_t offset{0};
  while (offset++ < 1000) {
    it = std::next(it);
    if (it == numbers.data.end()) {
      it = numbers.data.begin();
    }
  }
  result += std::get<0>(*it);
  while (offset++ < 2001) {
    it = std::next(it);
    if (it == numbers.data.end()) {
      it = numbers.data.begin();
    }
  }

  result += std::get<0>(*it);
  while (offset++ < 3002) {
    it = std::next(it);
    if (it == numbers.data.end()) {
      it = numbers.data.begin();
    }
  }

  result += std::get<0>(*it);

  return result;
}

auto part_two(const std::vector<std::string> &input) { return 2; }

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_20_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_20.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
