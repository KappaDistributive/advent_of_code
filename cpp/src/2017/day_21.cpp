#include <regex>  // NOLINT

#include "../utils/input.hpp"

static const std::regex rule_regex{"^(.*)\\s=>\\s(.*)$"};

auto apply_permutation(const std::string& image,
                       const std::vector<size_t>& permuntation) {
  assert(image.size() == permuntation.size());
  std::string result;
  for (auto entry : permuntation) {
    result.push_back(image[entry]);
  }

  return result;
}

auto rotate_clockwise(const std::string& image) {
  assert(image.size() == 4 || image.size() == 9);
  std::vector<size_t> permutation;
  if (image.size() == 4) {
    permutation = {2, 0, 3, 1};
  } else {
    permutation = {6, 3, 0, 7, 4, 1, 8, 5, 2};
  }

  return apply_permutation(image, permutation);
}

auto flip_vertically(const std::string& image) {
  assert(image.size() == 4 || image.size() == 9);
  std::vector<size_t> permutation;
  if (image.size() == 4) {
    permutation = {1, 0, 3, 2};
  } else {
    permutation = {2, 1, 0, 5, 4, 3, 8, 7, 6};
  }

  return apply_permutation(image, permutation);
}

auto get_symmetries(std::string image) {
  std::vector<std::string> symmetries;
  for (size_t rotation{0}; rotation < 4; ++rotation) {
    symmetries.push_back(image);
    image = rotate_clockwise(image);
  }
  image = flip_vertically(image);
  for (size_t rotation{0}; rotation < 4; ++rotation) {
    symmetries.push_back(image);
    image = rotate_clockwise(image);
  }

  return symmetries;
}

auto prepare_input(const std::vector<std::string>& input) {
  std::map<std::string, std::string> rules;
  std::smatch matches;
  for (auto line : input) {
    std::regex_match(line, matches, rule_regex);
    // ../.# => ##./#../...
    auto before = matches[1].str();
    auto after = matches[2].str();
    utils::replace_all_substrings(&before, "/", "");
    utils::replace_all_substrings(&after, "/", "");
    auto symmetries = get_symmetries(before);

    for (auto entry : symmetries) {
      if (rules.count(entry) > 0) {
        assertm(rules.at(entry) == after, "Conflicting rule!");
      } else {
        rules.insert({entry, after});
      }
    }
  }

  return rules;
}

auto step(const std::string& image,
          const std::map<std::string, std::string>& transformations) {
  size_t size{0};
  while (size * size < image.size()) {
    ++size;
  }
  assert(size * size == image.size());
  size_t block_size;
  size_t new_size;
  if (size % 2 == 0) {
    block_size = 2;
    new_size = (size / 2) * 3;
  } else {
    assert(size % 3 == 0);
    block_size = 3;
    new_size = (size / 3) * 4;
  }
  std::string result = std::string(new_size * new_size, ' ');
  for (size_t block_y{0}; block_y * block_size < size; ++block_y) {
    for (size_t block_x{0}; block_x * block_size < size; ++block_x) {
      std::string tmp;
      for (size_t y{0}; y < block_size; ++y) {
        for (size_t x{0}; x < block_size; ++x) {
          tmp.push_back(image[(block_y * block_size + y) * size +
                              block_x * block_size + x]);
        }
      }
      assert(tmp.size() == block_size * block_size);
      tmp = transformations.at(tmp);
      assert(tmp.size() == (block_size + 1) * (block_size + 1));
      for (size_t y{0}; y < block_size + 1; ++y) {
        for (size_t x{0}; x < block_size + 1; ++x) {
          result[((block_y * (block_size + 1) + y) * new_size) +
                 block_x * (block_size + 1) + x] =
              tmp[y * (block_size + 1) + x];
        }
      }
    }
  }

  return result;
}

auto print(const std::string& image) {
  size_t size{0};
  while (size * size < image.size()) {
    ++size;
  }
  assert(size * size == image.size());

  for (size_t y{0}; y < size; ++y) {
    for (size_t x{0}; x < size; ++x) {
      std::cout << image[y * size + x];
    }
    std::cout << std::endl;
  }
}

auto part_one(const std::vector<std::string>& input) {
  std::string image{".#...####"};
  auto transformations = prepare_input(input);
  for (size_t iteration{0}; iteration < 5; ++iteration) {
    image = step(image, transformations);
  }

  size_t result{0};
  for (auto character : image) {
    if (character == '#') {
      ++result;
    }
  }

  return result;
}

auto part_two(const std::vector<std::string>& input) {
  std::string image{".#...####"};
  auto transformations = prepare_input(input);
  for (size_t iteration{0}; iteration < 18; ++iteration) {
    image = step(image, transformations);
  }

  size_t result{0};
  for (auto character : image) {
    if (character == '#') {
      ++result;
    }
  }

  return result;
}

int main() {
  utils::Reader reader(std::filesystem::path("../../data/2017/input_21.txt"));
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two = part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
}

