#include <regex>  // NOLINT

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

struct Component {
  size_t in;
  size_t out;
  bool reversed;

  explicit Component(std::string& description) : reversed(false) {
    auto splits = utils::split_string(description, '/');
    assert(splits.size() == 2);
    this->in = std::stoull(splits[0]);
    this->out = std::stoull(splits[1]);
  }

  explicit Component(size_t in, size_t out, bool reversed = false)
      : in(in), out(out), reversed(reversed) {}

  friend std::ostream& operator<<(std::ostream& os,
                                  const Component& component) {
    if (component.reversed) {
      os << component.out << "/" << component.in;
    } else {
      os << component.in << "/" << component.out;
    }
    return os;
  }
};

auto prepare_input(const std::vector<std::string>& input) {
  std::vector<Component> components;
  for (auto line : input) {
    components.push_back(Component(line));
  }

  return components;
}

auto score(const std::vector<std::pair<size_t, bool>>& bridge,
           const std::vector<Component>& components) {
  size_t total_score{0};

  for (auto [index, _] : bridge) {
    total_score += components[index].in;
    total_score += components[index].out;
  }

  return total_score;
}

auto print(const std::vector<std::pair<size_t, bool>>& bridge,
           const std::vector<Component>& components) {
  std::cout << "Score " << score(bridge, components) << ": ";
  for (auto it{bridge.begin()}; it != bridge.end(); ++it) {
    auto [index, reversed] = *it;
    auto component = components[index];
    if (reversed) {
      component.reversed = !component.reversed;
    }
    std::cout << component;
    if (std::next(it) != bridge.end()) {
      std::cout << " --> ";
    }
  }
  std::cout << std::endl;
}

auto expand(const std::vector<std::vector<std::pair<size_t, bool>>>& bridges,
            const std::vector<Component>& components, bool part_two = false) {
  std::vector<std::vector<std::pair<size_t, bool>>> new_bridges;
  size_t max_score{0};
  bool made_progress{false};
  for (auto bridge : bridges) {
    bool used_bridge{false};
    for (size_t index{0}; index < components.size(); ++index) {
      bool valid{true};
      auto current_out =
          (bridge.back().second ? components[bridge.back().first].in
                                : components[bridge.back().first].out);
      if (components[index].in != current_out &&
          components[index].out != current_out) {
        continue;
      }

      for (auto [ref_index, _] : bridge) {
        if (ref_index == index) {
          valid = false;
          break;
        }
      }
      if (!valid) {
        continue;
      }

      auto new_bridge = bridge;
      if (components[index].in == current_out) {
        new_bridge.push_back(std::make_pair(index, false));
        used_bridge = true;
        max_score =
            std::max(max_score, part_two ? new_bridge.size()
                                         : score(new_bridge, components));
        made_progress = true;
      } else if (components[index].out == current_out) {
        new_bridge.push_back(std::make_pair(index, true));
        used_bridge = true;
        max_score =
            std::max(max_score, part_two ? new_bridge.size()
                                         : score(new_bridge, components));
        made_progress = true;
      }

      if (new_bridge.size() > bridge.size() &&
          std::find(new_bridges.begin(), new_bridges.end(), new_bridge) ==
              new_bridges.end()) {
        new_bridges.push_back(new_bridge);
      }
    }

    if (!used_bridge &&
        (part_two ? bridge.size() : score(bridge, components)) >= max_score) {
      new_bridges.push_back(bridge);
      max_score = part_two ? bridge.size() : score(bridge, components);
    }
  }
  return made_progress ? new_bridges
                       : std::vector<std::vector<std::pair<size_t, bool>>>();
}

auto part_one(const std::vector<std::string>& input) {
  auto components = prepare_input(input);
  std::vector<std::vector<std::pair<size_t, bool>>> bridges;

  for (size_t index{0}; index < components.size(); ++index) {
    if (components[index].in == 0) {
      bridges.push_back(
          std::vector<std::pair<size_t, bool>>{{std::make_pair(index, false)}});
    }
    if (components[index].out == 0) {
      bridges.push_back(
          std::vector<std::pair<size_t, bool>>{{std::make_pair(index, true)}});
    }
  }

  while (true) {
    auto new_bridges = expand(bridges, components);
    if (new_bridges.size() > 0) {
      bridges = new_bridges;
      // std::cout << bridges.size() << std::endl;
      size_t result{0};
      size_t best_index{0};
      for (size_t index{0}; index < bridges.size(); ++index) {
        auto bridge = bridges[index];
        auto current_score = score(bridge, components);
        if (current_score > result) {
          result = current_score;
          best_index = index;
        }
        // print(bridge, components);
      }
      // std::cout << "max score: " << result << std::endl;
      // print(bridges[best_index], components);

    } else {
      break;
    }
  }

  size_t result{0};
  for (auto bridge : bridges) {
    result = std::max(result, score(bridge, components));
    // print(bridge, components);
  }

  return result;
}

auto part_two(const std::vector<std::string>& input) {
  auto components = prepare_input(input);
  std::vector<std::vector<std::pair<size_t, bool>>> bridges;

  for (size_t index{0}; index < components.size(); ++index) {
    if (components[index].in == 0) {
      bridges.push_back(
          std::vector<std::pair<size_t, bool>>{{std::make_pair(index, false)}});
    } else if (components[index].out == 0) {
      bridges.push_back(
          std::vector<std::pair<size_t, bool>>{{std::make_pair(index, true)}});
    }
  }

  while (true) {
    auto new_bridges = expand(bridges, components, true);
    if (new_bridges.size() > 0) {
      bridges = new_bridges;
      // std::cout << bridges.size() << std::endl;
      size_t result{0};
      size_t best_index{0};
      for (size_t index{0}; index < bridges.size(); ++index) {
        auto bridge = bridges[index];
        auto current_score = score(bridge, components);
        if (current_score > result) {
          result = current_score;
          best_index = index;
        }
        // print(bridge, components);
      }
      // std::cout << "max score: " << result << std::endl;
      // print(bridges[best_index], components);

    } else {
      break;
    }
  }

  size_t best_distance{0};
  size_t best_score{0};
  for (auto bridge : bridges) {
    if (bridge.size() > best_distance) {
      best_score = score(bridge, components);
      best_distance = bridge.size();
    } else if (bridge.size() == best_distance) {
      best_score = std::max(best_score, score(bridge, components));
    }
    // print(bridge, components);
  }

  return best_score;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_24.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
}

