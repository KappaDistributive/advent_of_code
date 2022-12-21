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

  void move(std::__list_iterator<std::pair<int64_t, size_t>, void *> it,
            int64_t steps) {
    auto steps_to_take = steps % static_cast<int64_t>(this->data.size() - 1);
    if (steps_to_take < 0) {
      steps_to_take += static_cast<int64_t>(this->data.size() - 1);
    }
    if (steps_to_take == 0) {
      return;
    }
    auto target_it = it;
    for (int64_t step{0}; step < steps_to_take; ++step) {
      if (++target_it == this->data.end()) {
        target_it = this->data.begin();
      }
    }
    auto [value, index] = *it;
    this->data.erase(it);
    this->data.insert(std::next(target_it), std::make_pair(value, index));
  }

  void step(size_t index) {
    auto it = this->find_index(index);
    auto value = std::get<0>(*it);
    this->move(it, value);
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

auto part_two(const std::vector<std::string> &input) {
  Numbers numbers{input};
  for (auto it{numbers.data.begin()}; it != numbers.data.end(); ++it) {
    std::get<0>(*it) *= 811589153;
  }
  for (size_t round{0}; round < 10; ++round) {
    for (size_t step{0}; step < input.size(); ++step) {
      numbers.step(step);
    }
  }
  return numbers.result();
}

int main() {
  // std::filesystem::path input_path{"../../data/2022/input_20_mock.txt"};
  std::filesystem::path input_path{"../../data/2022/input_20.txt"};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  fmt::print("The answer to part one is: {}\n", part_one(input));
  fmt::print("The answer to part two is: {}\n", part_two(input));

  return 0;
}
