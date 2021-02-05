#include <cassert>

#include "../utils/input.hpp"

class GameOfChairs
{
private:
  int width, height;
  std::vector<char> chairs;

  int neighbor_count(size_t row, size_t column, char kind, bool part_two=false) const
  {
    int counter{0};
    if (!part_two)
    {
      size_t row_lower{(row > 0) ? row-1 : row},
             row_upper{(row + 1 < height) ? row + 1 : row},
             column_lower{(column > 0) ? column - 1 : column},
             column_upper{(column + 1 < width) ? column + 1 : column};
      for (size_t y{row_lower}; y <= row_upper; y++)
      {
        for (size_t x{column_lower}; x <= column_upper; x++)
        {
          if ((x != column || y != row) && this->get_seat(y, x) == kind)
          {
            counter++;
          }
        }
      }
    }
    else
    {
      for (int y_step{-1}; y_step <= 1; y_step++)
      {
        for (int x_step{-1}; x_step <= 1; x_step++)
        {
          if ((y_step == 0) && (x_step == 0))
          {
            continue;
          }

          size_t row_position{row}, column_position{column};
          while ((y_step == 0 || (y_step == -1 && row_position > 0) || (y_step == 1 && row_position + 1 < height)) &&
              (x_step == 0 || (x_step == -1 && column_position > 0) || (x_step == 1 && column_position + 1 < width)))
          {
            row_position += y_step;
            column_position += x_step;
            if(this->get_seat(row_position, column_position) != '.')
            {
              counter += this->get_seat(row_position, column_position) == kind;
              break;
            }
          }
        }
      }
    }
    return counter;
  }

public:
  explicit GameOfChairs(const std::vector<std::string>& seating_plan)
  {
    width = seating_plan[0].size();
    height = 0;

    for (auto line: seating_plan)
    {
      height++;
      assert (line.size() == width);
      for (auto seat: line)
      {
        chairs.push_back(seat);
      }
    }

    assert (chairs.size() == width * height);
  }

  char get_seat(size_t row, size_t column) const
  {
    if (row >= height)
    {
      throw std::out_of_range("Row is out of range.");
    }
    else if (column >= width)
    {
      throw std::out_of_range("Column is out of range.");
    }
    else
    {
      return chairs[row * width + column];
    }
  }

  bool step(bool part_two=false)
  {
    std::vector<std::pair<size_t, char>> changes;
    int threshold = part_two ? 5 : 4;

    for (size_t row{0}; row < height; row++)
    {
      for (size_t column{0}; column < width; column++)
      { 
        switch (chairs[row * width + column])
        {
          case 'L':
            if (neighbor_count(row, column, '#', part_two) == 0)
            {
              changes.push_back(std::make_pair(row * width + column, '#'));
            }
            break;
          case '#':
            if (neighbor_count(row, column, '#', part_two) >= threshold)
            {
              changes.push_back(std::make_pair(row * width + column, 'L'));
            }
            break;
          default: break;
        }
      }
    }

    for (auto [index, seat]: changes)
    {
      chairs[index] = seat;
    }

    return changes.size() > 0;
  }

  int count(char kind)
  {
    int counter{0};
    for (auto chair: chairs)
    {
      if (chair == kind)
      {
        counter++;
      }
    }
    return counter;
  }

  friend std::ostream& operator<< (std::ostream& os, const GameOfChairs& game)
  {
    for (size_t row{0}; row < game.height; row++)
    {
      for (size_t column{0}; column < game.width; column++)
      {
        os << game.get_seat(row, column);
      }
      if (row + 1 < game.height)
      {
        os << std::endl;
      }
    }

    return os;
  }
  
};

int part_one(const std::vector<std::string>& input)
{
  GameOfChairs game(input);
  do
  {
    // std:: cout << "\n" << game << std::endl;
  }
  while (game.step());

  return game.count('#');
}

int part_two(const std::vector<std::string>& input)
{

  GameOfChairs game(input);
  do
  {
    // std:: cout << "\n" << game << std::endl;
  }
  while (game.step(true));

  return game.count('#');
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_11.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
