#include <cmath>
#include <map>
#include <regex>
#include <vector>

#include "../utils/input.hpp"

class Node {
 public:
    std::pair<int, int> pos;
    size_t capacity, used;

    Node(std::pair<int, int> pos, size_t capacity, size_t used)
        : pos(pos), capacity(capacity), used(used) {
    }

    size_t available() const {
        return capacity - used;
    }

    size_t percentage_used() const {
        return static_cast<size_t>(std::round(100.f * static_cast<float>(used) / static_cast<float>(capacity)));
    }

    friend std::ostream& operator<<(std::ostream& os, const Node& node) {
        os << "/dev/grid/node-x" << node.pos.first << "-y" << node.pos.second
           << "\t" << node.capacity << "T"
           << "\t" << node.used << "T"
           << "\t" << node.available() << "T"
           << "\t" << node.percentage_used() << "%";

        return os;
    }
};

/*
* root@ebhq-gridcenter# df -h
* Filesystem              Size  Used  Avail  Use%
* /dev/grid/node-x0-y0     89T   67T    22T   75%
* /dev/grid/node-x0-y1     91T   72T    19T   79%
*/
std::map<std::pair<size_t, size_t>, Node> prepare_input(const std::vector<std::string>& input) {
    std::map<std::pair<size_t, size_t>, Node> nodes;
    std::regex fs_regex{"^/dev/grid/node-x(\\d+)-y(\\d+)\\s+(\\d+)T\\s+(\\d+)T.*$"};
    std::smatch matches;
    for (auto line: input) {
        if (std::regex_match(line, matches, fs_regex)) {
            size_t x = std::stol(matches[1]);
            size_t y = std::stol(matches[2]);
            size_t capacity = std::stol(matches[3]);
            size_t used = std::stol(matches[4]);
            nodes.insert({{x, y}, Node({x, y}, capacity, used)});
        }
    }

    return nodes;
}

size_t part_one(const std::vector<std::string>& input) {
    auto nodes = prepare_input(input);
    size_t viable_counter{0};
    size_t x_max{0}, y_max{0};

    for (auto [pos, node]: nodes) {
        x_max = std::max(x_max, pos.first);
        y_max = std::max(y_max, pos.second);
    }

    for (size_t y_a{0}; y_a <= y_max; y_a++) {
        for (size_t y_b{0}; y_b <= y_max; y_b++) {
            for (size_t x_a{0}; x_a <= x_max; x_a++) {
                for (size_t x_b{0}; x_b <= x_max; x_b++) {
                    if (x_a == x_b && y_a == y_b) {
                        continue;
                    }
                    auto a = nodes.at({x_a, y_a});
                    auto b = nodes.at({x_b, y_b});
                    if (a.used > 0 && a.used <= b.available()) {
                        viable_counter++;
                    }
                }
            }
        }
    }

    return viable_counter;
}


int part_two(const std::vector<std::string>& input) {
    return -2;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2016/data/input_22.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

