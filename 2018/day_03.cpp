#include <cassert>
#include <map>
#include <regex>
#include <set>

#include "../utils/input.hpp"

// #1359 @ 102,354: 13x23
//

typedef std::pair<int, int> Point;

class Claim {
 private:
     int id;
     Point position;
     Point shape;

 public:
     Claim(int id, int x, int y, int width, int height)
         : id(id), position({x, y}), shape({width, height}) {
     }

     explicit Claim(const std::string& input) {
         std::regex re{"^#(\\d+)\\s@\\s(\\d+),(\\d+):\\s(\\d+)x(\\d+)$"};
         std::smatch matches;
         std::regex_match(input, matches, re);
         assert(matches.size() == 6);
         id = std::stoi(matches[1]);
         position = {std::stoi(matches[2]), std::stoi(matches[3])};
         shape = {std::stoi(matches[4]), std::stoi(matches[5])};
     }

     std::set<Point> get_area() const {
         std::set<Point> area;

         for (int y{position.second}; y < position.second + shape.second; y++) {
            for (int x{position.first}; x < position.first + shape.first; x++) {
                area.insert({x, y});
            }
         }

         return area;
     }

     int get_id() const {
         return id;
     }

     bool operator<(const Claim& other) const {
         return this->id < other.id;
     }

     friend std::ostream& operator<<(std::ostream& os, const Claim& claim) {
         os << "#" << claim.id << " @ "
            << claim.position.first << "," << claim.position.second
            << ": " << claim.shape.first << "x" << claim.shape.second;
         return os;
     }
};

std::vector<Claim> prepare_claims(const std::vector<std::string>& input) {
    std::vector<Claim> claims;

    for (auto line : input) {
        claims.push_back(Claim(line));
    }

    return claims;
}

size_t part_one(const std::vector<std::string>& input) {
    auto claims = prepare_claims(input);
    std::map<Point, std::set<Claim>> claimed_areas;

    for (auto claim : claims) {
        for (auto point : claim.get_area()) {
            if (claimed_areas.count(point) == 0) {
                claimed_areas.insert(std::make_pair(point, std::set<Claim>()));
            }
            claimed_areas.at(point).insert(claim);
        }
    }

    size_t result{0};
    for (auto [point, claims] : claimed_areas) {
        if (claims.size() > 1) {
            result++;
        }
    }

    return result;
}

size_t part_two(const std::vector<std::string>& input) {
    auto claims = prepare_claims(input);
    std::map<Point, std::set<Claim>> claimed_areas;

    for (auto claim : claims) {
        for (auto point : claim.get_area()) {
            if (claimed_areas.count(point) == 0) {
                claimed_areas.insert(std::make_pair(point, std::set<Claim>()));
            }
            claimed_areas.at(point).insert(claim);
        }
    }

    for (auto claim : claims) {
        bool has_overlap{false};
        for (auto point : claim.get_area()) {
            if (claimed_areas.at(point).size() > 1) {
                has_overlap = true;
                break;
            }
        }
        if (!has_overlap) {
            return claim.get_id();
        }
    }

    return 0;
}



int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_03.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
