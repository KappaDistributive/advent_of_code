#include <algorithm>
#include <array>
#include <cassert>
#include <regex>  // NOLINT
#include <set>

#include "../utils/input.hpp"


enum class Type : int {
  generator,
  microchip
};


struct Component {
  std::string element;
  Type type;

  bool operator<(const Component& other) const noexcept {
    return this->element < other.element ||
           (this->element == other.element && this->type < other.type);
  }

  bool operator==(const Component& other) const noexcept {
    return this->element == other.element &&
           this->type == other.type;
  }
};


class Floors {
 private:
  std::array<std::set<Component>, 4> m_floors;
  size_t m_elevator;

  std::string
  abbr(std::string element,
       Type type) const {
    switch (type) {
      case Type::generator:
        return std::string {
          static_cast<char>(toupper(element[0]))} + "G";
        break;
      case Type::microchip:
        return std::string {
          static_cast<char>(toupper(element[0]))} + "M";
          break;
      default:
        throw std::runtime_error("This should never happen!");
        break;
    }
  }

  bool isPowered(const std::string& element) const noexcept {
    size_t microship_floor{0};
    bool found_floor{false};
    for (size_t floor{0};
         !found_floor && floor < this->m_floors.size();
         floor++) {
      for (auto component : this->m_floors[floor]) {
        if (component.element == element &&
            component.type == Type::microchip) {
          found_floor = true;
          microship_floor = floor;
          break;
        }
      }
    }
    assert(found_floor);

    for (auto component : this->m_floors[microship_floor]) {
      if (component.element == element &&
          component.type == Type::generator) {
            return true;
      }
    }

    return false;
  }

  bool hasGenerator(size_t floor) const {
    assert(floor < this->m_floors.size());
    for (auto component : this->m_floors[floor]) {
      if (component.type == Type::generator) {
        return true;
      }
    }

    return false;
  }

 public:
  explicit Floors(const std::vector<std::string>& input,
                  const bool part_two = false,
                  const bool verbose = false)
    : m_elevator(0) {
    std::regex generator_re{"(\\w+) generator"};
    std::regex microchip_re{"(\\w+)-compatible microchip"};
    std::smatch matches;
    for (size_t index{0}; index < input.size(); index++) {
      if (verbose) std::cout << "Floor: " << index + 1 << std::endl;
      auto line = input[index];
      auto tail = line;
      while (std::regex_search(tail, matches, generator_re)) {
        if (verbose) std::cout << "  Generator match: "
                               << matches[1].str() << std::endl;
        m_floors[index].insert(Component{matches[1].str(), Type::generator});
        tail = matches.suffix();
      }
      tail = line;
      while (std::regex_search(tail, matches, microchip_re)) {
        if (verbose) std::cout << "  Microchip match: "
                               << matches[1].str() << std::endl;
        m_floors[index].insert(Component{matches[1].str(), Type::microchip});
        tail = matches.suffix();
      }
    }
    if (part_two) {
      for (auto type : {Type::generator, Type::microchip}) {
        this->m_floors[0].insert(Component{"elerium", type});
        this->m_floors[0].insert(Component{"dilithium", type});
      }
    }
  }

  size_t numberComponents(size_t floor) const {
    assert(floor < this->m_floors.size());
    return this->m_floors[floor].size();
  }

  size_t numberComponents() const {
    size_t count{0};
    for (size_t floor{0}; floor < this->m_floors.size(); floor++) {
      count += this->numberComponents(floor);
    }

    return count;
  }

  std::set<Component> friedComponents() const {
    std::set<Component> fried_components;
    for (size_t floor{0}; floor < this->m_floors.size(); floor++) {
      if (this->hasGenerator(floor)) {
        for (auto component : this->m_floors[floor]) {
          if (component.type == Type::microchip &&
              !this->isPowered(component.element)) {
            fried_components.insert(component);
          }
        }
      }
    }

    return fried_components;
  }

  std::vector<Component> getComponents() const {
    std::vector<Component> components;
    for (auto floor : this->m_floors) {
      for (auto component : floor) {
        components.push_back(component);
      }
    }
    std::sort(components.begin(), components.end());

    return components;
  }

  bool move(const std::tuple<bool, Component, Component> move) {
    auto& [up, first, second] = move;
    if ((up && (this->m_elevator + 1 == this->m_floors.size())) ||
        (!up && this->m_elevator == 0) ||
        this->m_floors[this->m_elevator].count(first) == 0 ||
        this->m_floors[this->m_elevator].count(second) == 0) {
      return false;
    }
    this->m_floors[this->m_elevator].erase(first);
    this->m_floors[this->m_elevator].erase(second);
    if (up) {
      this->m_elevator++;
    } else {
      this->m_elevator--;
    }
    this->m_floors[this->m_elevator].insert(first);
    this->m_floors[this->m_elevator].insert(second);

    return this->friedComponents().size() == 0;
  }

  bool isEquivalent(const Floors& other) const noexcept {
    if (this->m_elevator != other.m_elevator ||
        this->m_floors.size() != other.m_floors.size()) {
      return false;
    }
    for (size_t index{0}; index < this->m_floors.size(); index++) {
      if (this->m_floors[index].size() != other.m_floors[index].size()) {
        return false;
      }
      size_t this_num_microships{0};
      size_t this_num_powered_microships{0};
      for (auto component : this->m_floors[index]) {
        if (component.type == Type::microchip) {
          this_num_microships++;
          if (this->isPowered(component.element)) {
            this_num_powered_microships++;
          }
        }
      }

      size_t other_num_microships{0};
      size_t other_num_powered_microships{0};
      for (auto component : other.m_floors[index]) {
        if (component.type == Type::microchip) {
          other_num_microships++;
          if (other.isPowered(component.element)) {
            other_num_powered_microships++;
          }
        }
      }

      if (this_num_microships != other_num_microships ||
          this_num_powered_microships != other_num_powered_microships) {
        return false;
      }
    }

    return true;
  }

  bool operator==(const Floors& other) const noexcept {
    return this->m_elevator == other.m_elevator &&
           this->m_floors == other.m_floors;
  }

  friend std::ostream& operator<<(std::ostream& os, const Floors& floors) {
    std::vector<std::string> elements;
    for (auto floor : floors.m_floors) {
      for (auto component : floor) {
        if (std::find(elements.begin(),
                      elements.end(),
                      component.element) == elements.end()) {
          elements.push_back(component.element);
        }
      }
    }
    std::sort(elements.begin(), elements.end());

    for (size_t index {0}; index < floors.m_floors.size(); index++) {
      os << "F" << floors.m_floors.size() - index << " "
         << (floors.m_floors.size() == floors.m_elevator + index + 1 ?
             'E' : '.');
      for (auto element : elements) {
        for (auto type : {Type::generator, Type::microchip}) {
          bool found{false};
          for (auto component :
               floors.m_floors[floors.m_floors.size() - index - 1]) {
            if (component.type == type &&
                component.element == element) {
              found = true;
              break;
            }
          }
          os << " " << (found ? floors.abbr(element, type) : " .");
        }
      }
      if (index + 1 < floors.m_floors.size()) os << std::endl;
    }

    return os;
  }
};


std::vector<Floors> prune(const std::vector<Floors>& candidates,
                          std::vector<Floors>* history) {
  std::vector<Floors> new_candidates;
  for (auto candidate : candidates) {
    bool found_equivalent{false};
    for (auto it = history->rbegin();
         it != history->rend();
         it++) {
      if (candidate.isEquivalent(*it)) {
        found_equivalent = true;
        break;
      }
    }
    if (!found_equivalent) {
      new_candidates.push_back(candidate);
      history->push_back(candidate);
    }
  }

  return new_candidates;
}

int run(const Floors& initial_floors) {
  std::vector<Floors> candidates = {initial_floors};
  std::vector<Floors> history = {initial_floors};  // prevent cycles
  const std::vector<Component> components = candidates[0].getComponents();
  std::vector<std::tuple<bool, Component, Component>> moves;
  for (bool up : {true, false}) {
    for (auto first : components) {
      for (auto second : components) {
        moves.push_back({up, first, second});
      }
    }
  }
  bool done{false};
  size_t num_moves{0};
  while (!done) {
    std::vector<Floors> new_candidates;
    for (auto candidate : candidates) {
      for (auto move : moves) {
        auto new_candidate = candidate;
        if (new_candidate.move(move)) {
          if (new_candidate.numberComponents(3) ==
              new_candidate.numberComponents()) {
            done = true;
          }
          new_candidates.push_back(new_candidate);
        }
      }
    }
    candidates = prune(new_candidates, &history);
    num_moves++;
  }
  return num_moves;
}


int
part_one(const std::vector<std::string>& input) {
  Floors floors(input);
  return run(floors);
}


int
part_two(const std::vector<std::string>& input) {
  Floors floors(input, true);
  return run(floors);
}


int
main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_11.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
