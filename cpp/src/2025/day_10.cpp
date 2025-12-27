#include "../utils/input.hpp"

struct Data {
  std::vector<bool> target;
  std::vector<std::vector<int>> buttons;
  std::vector<int> joltages;

  Data(const std::string &line) {
    auto splits = utils::split_string(line, ' ');
    for (size_t i{1}; i + 1 < splits[0].size(); ++i) {
      target.push_back(splits[0][i] == '#');
    }

    for (size_t i{1}; i + 1 < splits.size(); ++i) {
      std::vector<int> button;
      auto fragments =
          utils::split_string(splits[i].substr(1, splits[i].size() - 2), ',');
      for (const auto &frag : fragments) {
        button.push_back(std::stoi(frag));
      }
      buttons.push_back(button);
    }

    auto fragments = utils::split_string(
        splits.back().substr(1, splits.back().size() - 2), ',');
    for (const auto &frag : fragments) {
      joltages.push_back(std::stoi(frag));
    }
  }

  friend std::ostream &operator<<(std::ostream &os, const Data &data) {
    os << "[";
    for (const auto t : data.target) {
      os << (t ? '#' : '.');
    }
    os << "] ";
    for (const auto &button : data.buttons) {
      os << "(";
      for (size_t i = 0; i < button.size(); ++i) {
        os << button[i];
        if (i + 1 < button.size())
          os << ",";
      }
      os << ") ";
    }
    os << "{";
    for (size_t i = 0; i < data.joltages.size(); ++i) {
      os << data.joltages[i];
      if (i + 1 < data.joltages.size())
        os << ",";
    }
    os << "}";
    return os;
  }
};

std::vector<bool> push(std::vector<bool> state,
                       const std::vector<int> &button) {
  for (size_t i{0}; i < button.size(); ++i) {
    state[button[i]] = !state[button[i]];
  }
  return state;
}

std::vector<int> push(std::vector<int> state, const std::vector<int> &button) {
  for (size_t i{0}; i < button.size(); ++i) {
    ++state[button[i]];
  }
  return state;
}

int solve(const Data &data) {
  int result{0};
  std::vector<std::vector<bool>> states{
      std::vector<bool>(data.target.size(), false)};
  while (std::find(states.begin(), states.end(), data.target) == states.end()) {
    std::vector<std::vector<bool>> new_states;
    for (const auto &state : states) {
      for (const auto &button : data.buttons) {
        auto new_state = push(state, button);
        if (std::find(new_states.begin(), new_states.end(), new_state) ==
            new_states.end()) {
          new_states.push_back(new_state);
        }
      }
    }
    states = new_states;
    ++result;
  }
  return result;
}

int solve_two(const Data &data) {
  int result{0};
  std::vector<std::vector<int>> states{std::vector<int>(data.target.size(), 0)};
  while (std::find(states.begin(), states.end(), data.joltages) ==
         states.end()) {
    std::vector<std::vector<int>> new_states;
    for (const auto &state : states) {
      for (const auto &button : data.buttons) {
        auto new_state = push(state, button);
        bool add{true};
        for (size_t i{0}; i < new_state.size(); ++i) {
          if (new_state[i] > data.joltages[i]) {
            add = false;
            break; 
          }
        }
        if (add && std::find(new_states.begin(), new_states.end(), new_state) ==
            new_states.end()) {
          new_states.push_back(new_state);
        }
      }
    }
    states = new_states;
    ++result;
  }
  return result;
}

auto part_one(const std::vector<Data> &data) {
  return std::accumulate(data.begin(), data.end(), 0,
                         [](int acc, const Data &d) { return acc + solve(d); });
}

auto part_two(const std::vector<Data> &data) {
  int result{0};
  for (size_t i{4}; i < data.size(); ++i) {
    auto d{data[i]};
    auto temp{solve_two(d)};
    std::cout << "Result for " << d << " is " << temp << std::endl;
  }
  return result;
}

int main() {
  // std::filesystem::path input_path{"../../data/2025/input_10_mock.txt"};
  std::filesystem::path input_path{"../../data/2025/input_10.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::vector<Data> x;
  std::transform(data.begin(), data.end(), std::back_inserter(x),
                 [](const std::string &line) { return Data(line); });
  std::cout << std::format("The answer to part one is: {}", part_one(x))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two(x))
            << std::endl;

  return EXIT_SUCCESS;
}
