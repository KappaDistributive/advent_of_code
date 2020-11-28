#include <algorithm>
#include <regex>
#include <stdexcept>

#include "../utils/input.hpp"

class Box {
private:
  int length, width, height;

public:
  // description example: "4x23x21"
  explicit Box(std::string description) {
    std::regex re("^(\\d+)x(\\d+)x(\\d+)$");
    std::smatch match;

    std::regex_match(description, match, re);

    length = std::stoi(match[1]);
    width = std::stoi(match[2]);
    height = std::stoi(match[3]);
  }

  // get surface areas of faces in increasing order
  std::vector<int> surface_areas() {
    std::vector<int> surface_areas;
    surface_areas.reserve(3);
    surface_areas.push_back(length * width);
    surface_areas.push_back(length * height);
    surface_areas.push_back(width * height);
    std::sort(surface_areas.begin(), surface_areas.end());

    return surface_areas;
  }

  // get circumferences of faces in increasing order
  std::vector<int> circumferences() {
    std::vector<int> circumferences;
    circumferences.reserve(3);
    circumferences.push_back(2 * (length + width));
    circumferences.push_back(2 * (length + height));
    circumferences.push_back(2 * (width + height));
    std::sort(circumferences.begin(), circumferences.end());

    return circumferences;
  }

  int volume() {
    return length * width * height;
  }
};

std::vector<Box> get_boxes(std::vector<std::string> input) {
  std::vector<Box> boxes; 
  for (auto description: input)
    boxes.push_back(Box(description));

  return boxes;
}

int part_one(const std::vector<std::string>& input) {
  int result{0};
  auto boxes = get_boxes(input);
  for (auto box: boxes) {
    std::vector<int> surface_areas = box.surface_areas();
    result += surface_areas[0]; // slack
    for (auto surface_area: surface_areas)
      result += 2 * surface_area;
  }
  return result;
}

int part_two(const std::vector<std::string>& input) {
  int result{0};
  auto boxes = get_boxes(input);
  for (auto box: boxes) {
    result += box.volume();
    result += box.circumferences()[0]; // bow
  }
  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2015/data/input_02.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}