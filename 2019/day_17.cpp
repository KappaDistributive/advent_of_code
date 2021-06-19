#include <map>
#include <set>

#include "../utils/input.hpp"

#define assertm(exp, msg) assert(((void)msg, exp))

typedef std::pair<size_t, size_t> Point;


std::vector<int64_t> prepare_input(const std::vector<std::string>& input) {
  std::vector<int64_t> intcodes;
  std::transform(
    input.begin(),
    input.end(),
    std::back_inserter(intcodes),
      [](std::string code) -> int64_t {
        return std::strtoll(code.c_str(), NULL, 10);
      });
  return intcodes;
}

struct Instruction {
  int64_t opcode;
  std::vector<int64_t> parameters;
};


class CPU {
 private:
  std::map<size_t, int64_t> memory;
  size_t instruction_pointer;
  size_t relative_base;
  std::optional<int64_t> output;
  bool verbose;

 public:
  explicit CPU(const std::vector<int64_t>& intcodes,
               const bool& verbose = true)
      : instruction_pointer(0),
        relative_base(0),
        output(std::nullopt),
        verbose(verbose) {
    for (size_t index{0}; index < intcodes.size(); index++) {
      this->get_memory(index) = static_cast<int64_t>(intcodes[index]);
    }
  }

  int64_t get_mode(const Instruction& instruction, const size_t& index) {
    return static_cast<int64_t>(instruction.opcode /
    utils::pow(
      static_cast<size_t>(10),
      static_cast<size_t>(2 + index))) % 10;
  }

  int64_t& get_parameter(Instruction instruction, const size_t& index) {
    auto mode = get_mode(instruction, index);
    switch (mode) {
    case 0:   // position mode
      return this->get_memory(instruction.parameters[index]);
      break;
    case 1:  // immediate mode
      return instruction.parameters[index];
      break;
    case 2:  // relative mode
      return this->get_memory(relative_base +
        instruction.parameters[index]);
      break;
    default:
      throw std::runtime_error(
        "Invalid parameter mode " + std::to_string(mode));
      break;
    }
  }

bool execute(const Instruction& instruction) {
  bool halting{false};
  std::string input{""};
  bool update_instruction_pointer{true};

  switch (instruction.opcode % 100) {
    case 1:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) +
        get_parameter(instruction, 1);
      break;
    case 2:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) =
        get_parameter(instruction, 0) *
        get_parameter(instruction, 1);
      break;
    case 3:
      assert(instruction.parameters.size() == 1);
      std::cout << "Input required:" << std::endl;
      std::cin >> input;
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      assert(get_mode(instruction, 0) != 1);
      this->get_parameter(instruction, 0) =
        std::strtoll(input.c_str(), NULL, 10);
      break;
    case 4:
      assert(instruction.parameters.size() == 1);
      output = get_parameter(instruction, 0);
      if (verbose && output.has_value()) {
        std::cout << "Output: " << output.value() << std::endl;
      }
      break;
    case 5:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) != 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 6:
      assert(instruction.parameters.size() == 2);
      if (get_parameter(instruction, 0) == 0) {
        this->instruction_pointer = get_parameter(instruction, 1);
        update_instruction_pointer = false;
      }
      break;
    case 7:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      assert(get_mode(instruction, 2) != 1);
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) <
        get_parameter(instruction, 1));
      break;
    case 8:
      assert(instruction.parameters.size() == 3);
      assertm(get_mode(instruction, 2) != 1,
              "Parameters that an instruction writes to cannot be in immediate mode.");  // NOLINT
      this->get_parameter(instruction, 2) = static_cast<int64_t>(
        get_parameter(instruction, 0) ==
        get_parameter(instruction, 1));
      break;
    case 9:
      assert(instruction.parameters.size() == 1);
      relative_base += get_parameter(instruction, 0);
      break;
    case 99:
      halting = true;
      break;
    default:
      throw std::runtime_error(
        "Encountered invalid opcode: " +
        std::to_string(instruction.opcode));
      break;
  }

  if (update_instruction_pointer) {
    this->instruction_pointer += 1 + instruction.parameters.size();
  }

  return halting;
}

  bool execute() {
    int64_t opcode = this->get_memory(this->instruction_pointer);
    Instruction instruction;
    instruction.opcode = opcode;
    std::vector<int64_t> parameters;
    switch (opcode % 100) {
      case 1:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 2:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 3:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 4:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      case 5:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 6:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2) };
        break;
      case 7:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 8:
        parameters = {
          this->get_memory(this->instruction_pointer+1),
          this->get_memory(this->instruction_pointer+2),
          this->get_memory(this->instruction_pointer+3) };
        break;
      case 9:
        parameters = {this->get_memory(this->instruction_pointer+1)};
        break;
      default:
        parameters = {};
        break;
    }
    instruction.parameters = parameters;
    return execute(instruction);
  }

  int64_t run() {
    while (!execute()) {}
    return this->get_memory(0);
  }

  int64_t& get_memory(const size_t& index) {
    if (memory.count(index) == 0) {
      this->memory.insert({index, 0});
    }
    return this->memory.at(index);
  }

  std::optional<int64_t> get_output() const {
    return output;
  }

  void clear_output() {
    output = std::nullopt;
  }
};


enum class Orientation : int {
  north,
  east,
  south,
  west,
  tumbling
};


std::ostream&
operator<<(std::ostream& os, const Orientation& orientation) {
  switch (orientation) {
    case Orientation::north:
      os << '^';
      break;
    case Orientation::east:
      os << '>';
      break;
    case Orientation::south:
      os << 'v';
      break;
    case Orientation::west:
      os << '<';
      break;
    case Orientation::tumbling:
      os << 'X';
      break;
    default:
      throw std::runtime_error("This should never happen");
      break;
  }

  return os;
}


std::tuple<std::map<Point, char>, size_t, size_t>
create_map(const std::vector<int64_t>& intcodes) {
  std::map<Point, char> map;
  Point position{0, 0};
  size_t width{0}, height{1};
  CPU cpu(intcodes, false);
  while (!cpu.execute()) {
    auto output = cpu.get_output();
    if (output.has_value()) {
      if (static_cast<char>(output.value()) == '\n') {
        position.second++;
        position.first = 0;
        height++;
      } else {
        map.insert({position, static_cast<char>(output.value())});
        position.first++;
        width = std::max(width, position.first + 1);
      }
    }
    cpu.clear_output();
  }

  return std::make_tuple(map, width, height);
}


bool
is_intersection(const Point& point, const std::map<Point, char> map, size_t width, size_t height) {
  bool is_intersection{true};
  Point position;
  if (map.count(point) == 0 ||
      map.at(point) != '#' ||
      point.first == 0 ||
      point.first + 1 >= width ||
      point.second == 0 ||
      point.second + 1 >= height) {
    is_intersection = false;
  } else {
    for (int offset_y{-1}; is_intersection && offset_y <= 1; ++offset_y) {
      for (int offset_x{-1}; is_intersection && offset_x <= 1; ++offset_x) {
        if ((offset_y == 0 && offset_x != 0) || (offset_y != 0 && offset_x == 0)) {
          position.first = point.first + offset_x;
          position.second = point.second + offset_y;
          if (map.count(position) == 0 || map.at(position) != '#') {
            is_intersection = false;
          }
        }
      }
    }
  }

  return is_intersection;
}

std::set<Point>
calculate_intersections(const std::map<Point, char> map, size_t width, size_t height) {
  std::set<Point> intersections;

  for (size_t y{0}; y < height; ++y) {
    for (size_t x{0}; x < width; ++x) {
      Point position{x, y};
      if (is_intersection(position, map, width, height)) {
        intersections.insert(position);
      }
    }
  }

  return intersections;
}


void
print(const std::map<Point, char>& map, size_t width, size_t height) {
  Point position{0, 0};
  for (size_t y{0}; y < height; ++y) {
    for (size_t x{0}; x < width; ++x) {
      std::cout << (map.count(position) > 0 ? map.at(position) : '.');
      position.first++;
    }
    position.second++;
    position.first = 0;
    std::cout << '\n';
  }
  std::flush(std::cout);
}


auto
part_one(const std::vector<std::string>& input) {
  auto intcodes = prepare_input(input);
  auto [map, width, height] = create_map(intcodes);
  // print(map, width, height);
  auto intersections = calculate_intersections(map, width, height);
  size_t result{0};

  for (auto intersection : intersections) {
    // std::cout << "Intersection at (" << intersection.first << ", " << intersection.second << ")\n";
    result += intersection.first * intersection.second;
  }

  return result;
}


// auto
// part_two(const std::vector<std::string>& input) {
//   return 2;
// }


int main() {
  utils::Reader reader(std::filesystem::path("../2019/data/input_17.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ',');

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  // auto answer_two =  part_two(input);
  // std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

