#include <cassert>

#include "../utils/input.hpp"

class Map {
 private:
  std::vector<bool> map;
  int width, height;

 public:
  explicit Map(std::vector<std::string> input) {
    height = 0;
    width = input[0].size();
    for (auto line: input) {
      height++;
      assert(static_cast<size_t>(width) == line.size());

      for (auto mark: line) {
        switch (mark) {
          case '.': map.push_back(0); break;
          case '#': map.push_back(1); break;
          default: std::invalid_argument("Invalid marker encountered."); break;
        }
      }
    }
  }

  bool get_mark(int x, int y) const {
    return map[(y % height) * width + (x % width)];
  }

  int get_width() const {
    return width;
  }

  int get_height() const {
    return height;
  }
};

int get_encounters(const Map& map, int slope_x, int slope_y) {
  std::pair<int, int> position{0, 0};
  int result{0};

  while (std::get<1>(position) < map.get_height()) {
    std::get<0>(position) = (std::get<0>(position) + slope_x) % map.get_width();
    std::get<1>(position) += slope_y;
    result += static_cast<int>(map.get_mark(std::get<0>(position), std::get<1>(position)));
  }
  return result;
}


int part_one(std::vector<std::string> input) {
  Map map{input};

  return get_encounters(map, 3, 1);
}

int64_t part_two(std::vector<std::string> input) {
  Map map{input};
  int64_t result{1};
  std::vector<std::pair<int, int>> slopes = {
    std::make_pair(1, 1),
    std::make_pair(3, 1),
    std::make_pair(5, 1),
    std::make_pair(7, 1),
    std::make_pair(1, 2),
  };
  for (auto slope: slopes)
    result *= static_cast<int64_t>(get_encounters(map, std::get<0>(slope), std::get<1>(slope)));

  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_03.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

