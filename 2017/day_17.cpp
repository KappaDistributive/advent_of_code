#include <list>

#include "../utils/input.hpp"


void next(std::list<size_t>* buffer,
          std::list<size_t>::iterator* position,
          const size_t number_of_steps) {
    for (size_t step{0}; step < number_of_steps; step++) {
      if (std::next(*position) == buffer->end()) {
        *position = buffer->begin();
      } else {
        *position = std::next(*position);
      }
    }

    *position = buffer->insert(std::next(*position), buffer->size());
}


std::ostream&
operator<<(std::ostream& os,
           std::pair<std::list<size_t>*, std::list<size_t>::iterator*> state) {
  auto [buffer, position] = state;
  for (auto it = buffer->begin(); it != buffer->end(); it++) {
    os << ' ' << (it == *position ? '(' : ' ')
       << *it
       << (it == *position ? ')' : ' ') << ' ';
  }

  return os;
}


size_t
run(size_t number_of_steps,
    size_t number_of_rounds,
    size_t target) {
  bool verbose{false};
  std::list<size_t> buffer;
  buffer.push_back(0);
  auto position = buffer.begin();

  if (verbose) {
    std::cout << std::make_pair(&buffer, &position) << std::endl;
  }

  for (size_t round{1}; round <= number_of_rounds; round++) {
    next(&buffer, &position, number_of_steps);
    if (verbose) {
      std::cout << std::make_pair(&buffer, &position) << std::endl;
    }
  }

  auto it = buffer.begin();
  size_t result{0};
  for (; it != buffer.end(); it++) {
    if (*it == target) {
      result = std::next(it) == buffer.end() ? buffer.front() : *std::next(it);
      break;
    }
  }

  return result;
}


auto part_one(size_t number_of_steps) {
  return run(number_of_steps, 2017, 2017);
}


auto part_two(size_t  number_of_steps) {
  // takes ~10 minutes with release binaries
  number_of_steps = 30;
  return run(number_of_steps, 50000000, 0);
}


int main() {
  utils::Reader reader(std::filesystem::path("../2017/data/input_17.txt"));
  auto input = std::stoul(reader.get_lines()[0]);

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
