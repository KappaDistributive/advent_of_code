#include <cassert>
#include <sstream>
#include "utils/input.hpp"

#include "2015/day_01.hpp"

int main(int argc, char *argv[]) {
  assert(argc >= 3);

  std::string year = argv[1];
  std::string day = argv[2];
  std::string extension = argc > 3 ? ("_" + std::string{argv[3]}) : "";
  std::stringstream data_location;
  data_location << "../../data/" << year << "/input_" << day << extension << ".txt";

  utils::Reader reader{std::filesystem::path{data_location.str()}};
  auto input = reader.get_lines();
  auto target = std::make_pair(std::stoi(year), std::stoi(day));
  switch(target) {
    case "2015":year_2015::day_01::part_one(input);
  }
  std::cout << "The answer to part one is: " << answer_part_one << std::endl;
  auto answer_part_two = year_2015::day_01::part_two(input);
  std::cout << "The answer to part two is: " << answer_part_two << std::endl;
}
