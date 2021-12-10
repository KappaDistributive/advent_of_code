#include "../utils/input.hpp"

class Walker {
 private:
  std::pair<int, int> position;
  std::vector<std::pair<int, int>> path;

 public:
  Walker()
      : position(std::pair<int, int>{0, 0}),
        path({std::pair<int, int>{0, 0}}) {}

  const std::vector<std::pair<int, int>>& get_path() const { return path; }

  std::pair<int, int> step(char direction) {
    switch (direction) {
      case '^':
        std::get<1>(position)++;
        break;
      case 'v':
        std::get<1>(position)--;
        break;
      case '<':
        std::get<0>(position)--;
        break;
      case '>':
        std::get<0>(position)++;
        break;
      default:
        throw std::invalid_argument("Invalid direction.");
        break;
    }
    path.push_back(position);

    return position;
  }
};

std::vector<char> get_directions(const std::string& input) {
  std::vector<char> directions;

  for (auto direction : input) directions.push_back(direction);

  return directions;
}

int part_one(const std::string& input) {
  std::vector<char> directions = get_directions(input);
  Walker santa;

  for (auto direction : directions) santa.step(direction);

  std::set<std::pair<int, int>> unique_positions;
  for (auto position : santa.get_path()) unique_positions.insert(position);

  return static_cast<int>(unique_positions.size());
}

int part_two(const std::string& input) {
  std::vector<char> directions = get_directions(input);
  Walker santa;
  Walker robo;

  int counter{0};

  for (auto direction : directions) {
    switch (counter % 2) {
      case 0:
        santa.step(direction);
        break;
      case 1:
        robo.step(direction);
        break;
      default:
        throw std::invalid_argument("This should never happen.");
        break;
    }
    counter++;
  }

  std::set<std::pair<int, int>> unique_positions;
  for (auto position : santa.get_path()) unique_positions.insert(position);
  for (auto position : robo.get_path()) unique_positions.insert(position);

  return static_cast<int>(unique_positions.size());
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2015/input_03.txt"));
  std::string input = reader.get_lines()[0];

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}
