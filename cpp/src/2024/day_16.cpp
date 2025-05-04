#include "../utils/geometry.hpp"
#include "../utils/input.hpp"
#include <sstream>

using Point = utils::geometry::Point<int, 2>;

struct Reindeer {
  Point position;
  Point direction;

  char show() const noexcept {
    if (this->direction[0] == 0 && this->direction[1] == -1) {
      return '^';
    } else if (this->direction[0] == 0 && this->direction[1] == 1) {
      return 'v';
    } else if (this->direction[0] == -1 && this->direction[1] == 0) {
      return '<';
    } else if (this->direction[0] == 1 && this->direction[1] == 0) {
      return '>';
    }
    return 'R';
  }
};

class Map {
 private:
   int m_width, m_height;
   std::vector<std::string> m_data;
   Point m_start, m_end;
 public:
   Map(const std::vector<std::string> &data) {
     this->m_height = data.size();
     this->m_width = data[0].size();
     this->m_data.reserve(this->m_height);
     for (int y{0}; y < this->m_height; y++) {
       std::string line;
       line.reserve(this->m_width);
       for (int x{0}; x < this->m_width; x++) {
         char c = data[y][x];
         if (c == 'E') {
           this->m_end = Point{{x, y}};
           line.push_back('.');
         } else if (c == 'S') {
           this->m_start = Point{{x, y}};
           line.push_back('.');
         } else {
           line.push_back(c);
         }
       }
       assert (line.size() == this->m_width);
       this->m_data.push_back(line);
     }
   }

   char at(int x, int y) const noexcept {
     if (x < 0 || x >= this->m_width || y < 0 || y >= this->m_height) {
       return ' ';
     }
     return this->m_data[y][x];
   }

   std::ostringstream show(const std::vector<Reindeer>& path = {}) const noexcept {
     std::ostringstream os;
     for (int y{0}; y < this->m_height; ++y) {
       for (int x{0}; x < this->m_width; ++x) {
         Point position{{x, y}};
         auto it = std::find_if(path.cbegin(), path.cend(), [&position](const Reindeer& reindeer) {
           return reindeer.position == position;
         });
         if (it != path.cend()) {
           os << it->show();
         } else {
           if (position == this->m_start || position == this->m_end) {
             os << "\033[3m";;
           }
           os << this->at(x, y);
          if (position == this->m_start || position == this->m_end) {
             os << "\033[23m";
           }
         }
       }
       os << '\n';
     }
     return os;
   }
};

auto part_one(const std::vector<std::string> &data) {
  Map map(data);
  std::cout << map.show().str() << std::endl;
  return 1;
}

auto part_two() {
  return 2;
}

int main() {
  std::filesystem::path input_path{"../../data/2024/input_16_mock.txt"};
  // std::filesystem::path input_path{"../../data/2024/input_16.txt"};
  utils::Reader reader(input_path);
  auto data = reader.get_lines();
  std::cout << std::format("The answer to part one is: {}", part_one(data))
            << std::endl;
  std::cout << std::format("The answer to part two is: {}", part_two())
            << std::endl;

  return EXIT_SUCCESS;
}
