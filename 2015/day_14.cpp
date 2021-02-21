#include <cassert>
#include <regex>

#include "../utils/input.hpp"

class Reindeer {
 private:
  std::string name;
  int speed;
  int stamina;
  int rest;

  int clock;
  int distance;

 public:
  explicit Reindeer(const std::string& description)
      : clock(0), distance(0) {
    std::regex re{"^(\\w+)\\D*(\\d+)\\D*(\\d+)\\D*(\\d+).*$"};
    std::smatch matches;

    std::regex_match(description, matches, re);
    assert(matches.size() == 5);

    name = matches[1].str();
    speed = std::stoi(matches[2].str());
    stamina = std::stoi(matches[3].str());
    rest = std::stoi(matches[4].str());
  }

  int step() {
    auto stage = clock % (stamina + rest);
    if (stage < stamina) {
      distance += speed;
    }
    clock++;
    return distance;
  }

  int get_distance() const {
    return distance;
  }

  friend std::ostream& operator<< (std::ostream& os, const Reindeer& reindeer) {
    os << reindeer.name << " can fly " << reindeer.speed << " km/s for " << reindeer.stamina << " seconds, but then must rest for " << reindeer.rest << " seconds.";
    return os;
  }
};

std::vector<Reindeer> create_reindeers(const std::vector<std::string>& input) {
  std::vector<Reindeer> reindeers;
  for (auto line: input) {
    reindeers.push_back(Reindeer(line));
  }

  return reindeers;
}

int part_one(const std::vector<std::string>& input) {
  int result{0};
  auto reindeers = create_reindeers(input);

  for (size_t clock{0}; clock < 2503; clock++) {
    for (size_t index{0}; index < reindeers.size(); index++) {
      reindeers[index].step();
    }
  }

  for (size_t index{0}; index < reindeers.size(); index++) {
    if (reindeers[index].get_distance() > result) {
      result = reindeers[index].get_distance();
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input) {
  auto reindeers = create_reindeers(input);
  std::vector<int> scores;
  for (size_t index{0}; index < reindeers.size(); index++) {
    scores.push_back(0);
  }

  for (size_t clock{0}; clock < 2503; clock++) {
    for (size_t index{0}; index < reindeers.size(); index++) {
      reindeers[index].step();
    }
    size_t max_index{0};
    for (size_t index{1}; index < reindeers.size(); index++) {
      if (reindeers[index].get_distance() > reindeers[max_index].get_distance()) {
        max_index = index;
      }
    }
    scores[max_index]++;
  }

  int result{0};
  for (size_t index{0}; index < scores.size(); index++) {
    if (scores[index] > result) {
      result = scores[index];
    }
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_14.txt"));
  auto input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

