#include <limits>

#include "../utils/geometry.hpp"
#include "../utils/input.hpp"

using Point = utils::geometry::Point<int, 2>;

class Image {
 private:
  std::set<Point> m_lit_pixels;
  std::array<bool, 512> m_enhancement_algorithm;
  Point m_north_west, m_south_east;
  bool m_default_value{false};

  auto pixel(const Point& point) const noexcept {
    auto north_west_coordinates = this->m_north_west.coordinates();
    auto south_east_coordinates = this->m_south_east.coordinates();
    auto coordinates = point.coordinates();
    if (north_west_coordinates[0] <= coordinates[0] &&
        coordinates[0] <= south_east_coordinates[0] &&
        north_west_coordinates[1] <= coordinates[1] &&
        coordinates[1] <= south_east_coordinates[1]) {
      return this->m_lit_pixels.count(point) > 0;
    }

    return this->m_default_value;
  }

  auto pixel_to_index(const Point& point) const noexcept {
    size_t index{0};
    for (int offset_y{-1}; offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; offset_x <= 1; ++offset_x) {
        index <<= 1;
        Point offset{std::array<int, 2>{offset_x, offset_y}};
        index += this->pixel(point + offset);
      }
    }

    return index;
  }

 public:
  explicit Image(const std::vector<std::string>& input) {
    assertm(input.size() > 0 && input[0].size() == 512,
            "Invalid enhancement algorithm detected.");

    for (size_t index{0}; index < input[0].size(); ++index) {
      this->m_enhancement_algorithm[index] = input[0][index] == '#';
    }

    int y{0}, x{0};
    for (size_t index{2}; index < input.size(); ++index) {
      x = 0;

      for (auto character : input[index]) {
        if (character == '#') {
          this->m_lit_pixels.insert(Point{std::array<int, 2>{x, y}});
        }
        ++x;
      }

      ++y;
    }

    this->m_north_west = Point{std::array<int, 2>{0, 0}};
    this->m_south_east = Point{std::array<int, 2>{x - 1, y - 1}};
  }

  auto brightness() const noexcept {
    if (this->m_default_value) {
      return -1;
    }
    return static_cast<int>(this->m_lit_pixels.size());
  }

  void enhance() noexcept {
    std::set<Point> lit_pixels;

    auto north_west_coordinates = this->m_north_west.coordinates();
    auto south_east_coordinates = this->m_south_east.coordinates();

    for (int y{north_west_coordinates[1] - 1};
         y <= south_east_coordinates[1] + 1; ++y) {
      for (int x{north_west_coordinates[0] - 1};
           x <= south_east_coordinates[0] + 1; ++x) {
        Point point{std::array<int, 2>{x, y}};
        if (this->m_enhancement_algorithm[this->pixel_to_index(point)]) {
          lit_pixels.insert(point);
        }
      }
    }
    this->m_north_west += Point{std::array<int, 2>{-1, -1}};
    this->m_south_east += Point{std::array<int, 2>{1, 1}};

    this->m_lit_pixels = lit_pixels;
    if (this->m_default_value) {
      this->m_default_value = this->m_enhancement_algorithm[0x1ff];
    } else {
      this->m_default_value = this->m_enhancement_algorithm[0];
    }
  }

  friend std::ostream& operator<<(std::ostream& os, const Image& image) {
    auto north_west_coordinates = image.m_north_west.coordinates();
    auto south_east_coordinates = image.m_south_east.coordinates();

    for (int y{north_west_coordinates[1] - 1};
         y <= south_east_coordinates[1] + 1; ++y) {  // y at north < y at south
      for (int x{north_west_coordinates[0] - 1};
           x <= south_east_coordinates[0] + 1; ++x) {
        Point pixel{std::array<int, 2>{x, y}};
        os << (image.pixel(pixel) ? '#' : '.');
      }
      os << '\n';
    }
    return os;
  }
};

auto part_one(const std::vector<std::string>& input) {
  Image image{input};

  // std::cout << image << std::endl;
  image.enhance();
  // std::cout << image << std::endl;
  image.enhance();
  // std::cout << image << std::endl;

  return image.brightness();
}

auto part_two(const std::vector<std::string>& input) {
  Image image{input};

  for (size_t index{0}; index < 50; ++index) {
    image.enhance();
  }

  return image.brightness();
}

int main(int argc, char* argv[]) {
  std::string extension{""};

  if (argc > 1) {
    extension = "_" + std::string(argv[1]);
  }
  std::filesystem::path input_path{
      fmt::format("../../data/2021/input_20{}.txt", extension)};
  utils::Reader reader(input_path);
  auto input = reader.get_lines();

  auto answer_one = part_one(input);
  fmt::print("The answer to part one is: {}\n", answer_one);
  auto answer_two = part_two(input);
  fmt::print("The answer to part two is: {}\n", answer_two);

  return 0;
}
