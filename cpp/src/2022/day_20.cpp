#include "../utils/input.hpp"

struct Numbers {
  std::list<std::pair<int64_t, size_t>> data;

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

  auto find_index(size_t index) {
    auto it{this->data.begin()};
    for (; it != this->data.end(); ++it) {
      if (std::get<1>(*it) == index) {
        return it;
      }
    }
    throw std::out_of_range("");
  }

  auto move(std::__list_iterator<std::pair<int64_t, size_t>, void *> it,
            const int64_t steps) {
    int64_t steps_taken{0};
    if (steps >= 0) {
      it = this->data.erase(it);
      while (steps_taken++ < steps) {
        it = std::next(it);
        if (it == this->data.end()) {
          it = this->data.begin();
        }
      }
    } else {
      it = this->data.erase(it);
      while (steps_taken++ < -steps) {
        it = std::prev(it);
        if (it == this->data.begin()) {
          it = this->data.end();
        }
      }
    }

    return it;
  }

  void step(size_t index) {
    auto it = this->find_index(index);
    auto value = std::get<0>(*it);
    it = this->move(it, value);
    this->data.insert(it, std::make_pair(value, index));
  }

  auto result() const {
    auto it{this->data.cbegin()};
    while (std::get<0>(*it) != 0) {
      it = std::next(it);
      if (it == this->data.cend()) {
        it = this->data.cbegin();
      }
    }
    int64_t result{0};
    size_t offset{0};
    while (offset++ < 1000) {
      it = std::next(it);
      if (it == this->data.cend()) {
        it = this->data.cbegin();
      }
    }
    result += std::get<0>(*it);
    while (offset++ < 2001) {
      it = std::next(it);
      if (it == this->data.cend()) {
        it = this->data.cbegin();
      }
    }
    result += std::get<0>(*it);
    while (offset++ < 3002) {
      it = std::next(it);
      if (it == this->data.cend()) {
        it = this->data.cbegin();
      }
    }
    result += std::get<0>(*it);
    return result;
  }
};

auto part_one(const std::vector<std::string> &input) {
  Numbers numbers{input};
  for (size_t step{0}; step < numbers.data.size(); ++step) {
    numbers.step(step);
  }
  return numbers.result();
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
