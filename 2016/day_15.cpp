#include <cassert>
#include <regex>

#include "../utils/input.hpp"


class Disc
{
private:
  size_t id, initial_position, size;

public:
  Disc(size_t id, size_t size, size_t initial_position)
    : id(id), size(size), initial_position(initial_position)
  {

  }

  size_t get_position(size_t time) const
  {
    return (this->initial_position + time) % this->size;
  }

  friend std::ostream& operator<< (std::ostream& os, const Disc& disc)
  {
    os << "Disc #" << disc.id << " has " << disc.size << " positions;"
       << " at time=0 it is at position " << disc.initial_position << ".";
    return os;
  }
};

std::vector<Disc> prepare_input (const std::vector<std::string>& input)
{
  std::regex re{"^Disc\\s#(\\d+)\\shas\\s(\\d+)\\spositions;\\sat\\stime=0,\\sit\\sis\\sat position\\s(\\d+).$"};
  std::smatch matches;
  std::vector<Disc> discs;
  for (auto line: input)
  {
    assert(std::regex_match(line, matches, re));
    discs.push_back(Disc(std::stoi(matches[1].str()), std::stoi(matches[2].str()), std::stoi(matches[3].str())));
  }
  return discs;
}

size_t find_first_opportune_moment(const std::vector<Disc>& discs)
{
  size_t global_time{0}, local_time{0};
  bool searching{true};
  while (searching)
  {
    local_time = global_time;
    searching = false;
    for (auto disc: discs)
    {
      local_time++;
      if (disc.get_position(local_time) != 0)
      {
        searching = true;
        break;
      }
    }
    global_time++;
  }
  return global_time - 1;
}

size_t part_one(const std::vector<std::string>& input)
{
  auto discs = prepare_input(input);
  return find_first_opportune_moment(discs);
}

int part_two(const std::vector<std::string>& input)
{
  auto discs = prepare_input(input);
  discs.push_back(Disc(discs.size(), 11, 0));
  return find_first_opportune_moment(discs);
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_15.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
