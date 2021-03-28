#include <array>
#include <cassert>
#include <vector>

#include "../utils/input.hpp"

static const size_t WIDTH = 25;
static const size_t HEIGHT = 6;

std::vector<std::array<int, WIDTH * HEIGHT>>
prepare_input(const std::string& input) {
  std::vector<std::array<int, WIDTH * HEIGHT>> image;

  for (size_t index{0}; index < input.size(); index++) {
    assert('0' <= input[index] && input[index] <= '2');
    size_t layer = index / (WIDTH * HEIGHT);
    if (layer == image.size()) {
        image.push_back(std::array<int, WIDTH * HEIGHT>());
    }
    size_t y = (index % (WIDTH * HEIGHT)) / WIDTH;
    size_t x = index % WIDTH;
    assert(index == layer * WIDTH * HEIGHT + y * WIDTH + x);
    image[layer][y * WIDTH + x] = input[index] - '0';
}

  return image;
}

size_t part_one(const std::string& input) {
  auto image = prepare_input(input);

  size_t num_zeros;
  size_t layer_index{0};
  for (size_t index{0}; index < image.size(); index++) {
    auto layer = image[index];
    size_t num_zeros_in_layer{0};
    for (auto pixel : layer) {
      if (pixel == 0) {
        num_zeros_in_layer++;
      }
    }
    if (layer_index == 0 || num_zeros_in_layer < num_zeros) {
      layer_index = index;
      num_zeros = num_zeros_in_layer;
    }
  }
  size_t num_ones{0}, num_twos{0};
  for (auto pixel : image[layer_index]) {
    if (pixel == 1) {
      num_ones++;
    } else if (pixel == 2) {
      num_twos++;
    }
  }
  return num_ones * num_twos;
}

int part_two(const std::string& input) {
  return 6;
}

int main() {
  utils::Reader reader(
    std::filesystem::path("../2019/data/input_08.txt"));
  auto input = reader.get_lines()[0];

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

