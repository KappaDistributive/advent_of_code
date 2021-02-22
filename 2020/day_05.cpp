#include <algorithm>
#include <cassert>

#include "../utils/input.hpp"

class BoardingPass {
 private:
  std::string code;
  int row, column;

  int partition(const std::string& code, int lower, int upper) {
    if (code.size() > 0) {
      std::string new_code = code.substr(1, code.size()-1);
      switch (code[0]) {
        case 'F': return partition(new_code, lower, lower - 1 + (upper + 1 - lower)/2); break;
        case 'B': return partition(new_code, lower + (upper + 1 - lower)/ 2, upper); break;
        case 'L': return partition(new_code, lower, lower - 1 + (upper + 1 - lower)/2); break;
        case 'R': return partition(new_code, lower + (upper + 1 - lower)/ 2, upper); break;
        default: throw std::runtime_error("Invalid code: " + code); break;
      }
    }

    return lower;
  }

 public:
  explicit BoardingPass(const std::string& code) : code(code) {
    row = partition(code.substr(0, 7), 0, 127);
    column = partition(code.substr(7, 3), 0, 7);
  }

  int get_row() const { return row; }
  int get_column() const { return column; }
  int get_id() const { return get_row() * 8 + get_column(); }
};

std::ostream& operator << (std::ostream& os, const BoardingPass& boarding_pass) {
  os << "row: " << boarding_pass.get_row()
     << ", column: " << boarding_pass.get_column()
     << ", id: " << boarding_pass.get_id();

  return os;
}

int part_one(const std::vector<std::string>& input) {
  int result{0};
  for (auto line: input) {
    BoardingPass boarding_pass(line);
    if (boarding_pass.get_id() > result) {
      result = boarding_pass.get_id();
    }
  }
  return result;
}

int part_two(const std::vector<std::string>& input) {
  std::vector<int> ids;
  for (auto line: input) {
    ids.push_back(BoardingPass(line).get_id());
  }
  std::sort(ids.begin(), ids.end());

  for (size_t index{1}; index < ids.size(); index++) {
    if (ids[index] != ids[index-1]+1) {
      assert(ids[index] = ids[index-1]+2);
      return ids[index] - 1;
    }
  }

  return -1;
}

int main() {
  utils::Reader reader(std::filesystem::path("../2020/data/input_05.txt"));
  std::vector<std::string> input = reader.get_lines();

  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;

  return 0;
}

