#include <algorithm>
#include <cassert>
#include <functional>
#include <limits>
#include <list>
#include <unordered_set>

#include "../utils/input.hpp"

class ClosedInterval
{
private:
  unsigned int lower_bound, upper_bound;

public:
  ClosedInterval(const unsigned int& lower_bound, const unsigned int& upper_bound)
    : lower_bound(lower_bound), upper_bound(upper_bound)
  {
  }

  ClosedInterval(const std::string& range)
  {
    auto splits = utils::split_string(range, '-');
    assert (splits.size() == 2);
    lower_bound = std::stoul(splits[0]);
    upper_bound = std::stoul(splits[1]);
  }

  bool intersects(const ClosedInterval& other) const
  {
    return (
        (this->lower_bound >= other.lower_bound && this->lower_bound <= other.upper_bound) ||   // ( [ ) ] || ( [ ] )
        (this->upper_bound >= other.lower_bound && this->upper_bound <= other.upper_bound) ||   // [ ( ] ) || ( [ ] )
        (other.lower_bound >= this->lower_bound && other.lower_bound <= this->upper_bound) ||   // [ ( ] ) || [ ( ) ]
        (other.upper_bound >= this->lower_bound && other.upper_bound <= this->upper_bound)      // ( [ ) ] || [ ( ) ]
    );
  }

  bool merge(const ClosedInterval& other)
  {
    if (intersects(other))
    {
      this->lower_bound = std::min(this->lower_bound, other.lower_bound);
      this->upper_bound = std::min(this->upper_bound, other.upper_bound);
      return true;
    }
    else
    {
      return true;
    }
  }

  unsigned int get_lower_bound() const
  {
    return this->lower_bound;
  }

  unsigned int get_upper_bound() const
  {
    return this->upper_bound;
  }

  friend std::ostream& operator<< (std::ostream& os, const ClosedInterval& interval)
  {
    os << "[" << interval.lower_bound << ", " << interval.upper_bound << "]";
    return os;
  }
};

class ClosedIntervalEqual
{
public:
  bool operator() (const ClosedInterval& lhs, const ClosedInterval& rhs) const
  {
    return lhs.get_lower_bound() == rhs.get_lower_bound() && lhs.get_upper_bound() == rhs.get_upper_bound();
  }
};

class ClosedIntervalHash
{
public:
  size_t operator() (const ClosedInterval& interval) const
  {
    return std::hash<unsigned int>{}(interval.get_lower_bound()) ^ std::hash<unsigned int>{}(interval.get_upper_bound());
  }
};

unsigned int part_one(const std::vector<std::string>& input)
{
  std::unordered_set<ClosedInterval, ClosedIntervalHash, ClosedIntervalEqual> intervals;

  for (auto line: input)
  {
    intervals.insert(ClosedInterval(line));
  }

  unsigned int result{0};
  bool conflict{true};

  while (conflict)
  {
    conflict = false;
    for (auto interval: intervals)
    {
      if (interval.get_lower_bound() <= result && result <= interval.get_upper_bound())
      {
        result = interval.get_upper_bound() + 1;
        conflict = true;
      }
    }
  }

  return result;
}

unsigned int part_two(const std::vector<std::string>& input)
{
  std::vector<ClosedInterval> sorted_intervals;
  for (auto line: input)
  {
    sorted_intervals.push_back(ClosedInterval(line));
  }
  std::sort(
    sorted_intervals.begin(), sorted_intervals.end(),
    [](const ClosedInterval& lhs, const ClosedInterval& rhs) -> bool
    {
      return lhs.get_lower_bound() < rhs.get_lower_bound();
    }
  );

  for (size_t index{0}; index+1 < sorted_intervals.size(); index++)
  {
    assert (sorted_intervals[index].get_lower_bound() < sorted_intervals[index+1].get_lower_bound());
  }

  unsigned int result{0};
  unsigned int lowest_available{0};
  unsigned int highest_bound{0};

  for (auto interval: sorted_intervals)
  {
    if (interval.get_lower_bound() > lowest_available)
    {
      result += interval.get_lower_bound() - lowest_available;
    }
    if (lowest_available <= interval.get_upper_bound())
    {
      lowest_available = interval.get_upper_bound();
      highest_bound = interval.get_upper_bound();
      if (lowest_available < std::numeric_limits<unsigned int>::max())
      {
        lowest_available++;
      }
    }
  }
  if (highest_bound < std::numeric_limits<unsigned int>::max())
  {
    result += std::numeric_limits<unsigned int>::max() - highest_bound;
  }

  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2016/data/input_20.txt"));
  auto input = reader.get_lines();
  
  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}
