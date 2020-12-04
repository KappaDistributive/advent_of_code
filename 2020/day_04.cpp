#include <regex>

#include "../utils/input.hpp"

class Passport
{
private:
  bool valid;
  std::regex re_birth_year{"^(?:.*\\s)?byr:(\\S+)(?:\\s.*)?$"},
             re_issue_year{"^(?:.*\\s)?iyr:(\\S+)\\s.*$"},
             re_expiration_year{"^(?:.*\\s)?eyr:(\\S+)(?:\\s.*)?$"},
             re_height{"^(?:.*\\s)?hgt:(\\S+)(?:\\s.*)?$"},
             re_hair_color{"^(?:.*\\s)?hcl:(\\S+)(?:\\s.*)?$"},
             re_eye_color{"^(?:.*\\s)?ecl:(\\S+)(?:\\s.*)?$"},
             re_passport_id{"^(?:.*\\s)?pid:(\\S+)(?:\\s.*)?$"},
             re_country_id{"^(?:.*\\s)?cid:(\\S+)(?:\\s.*)?$"};
  std::smatch matches;

  bool valid_year(const std::string& candidate, int lower_bound, int upper_bound) const 
  {

    if(!std::all_of(candidate.begin(), candidate.end(), ::isdigit) || candidate.size() != 4) {
      return false;
    }
    else if(std::stoi(candidate) < lower_bound || std::stoi(candidate) > upper_bound)
    {
      return false;
    }
    return true;
  }

public:
  std::string birth_year, issue_year, expiration_year, height, hair_color, eye_color, passport_id, country_id;
  explicit Passport(std::string data) : valid(true)
  {
    std::regex_match(data, matches, re_birth_year);
    if(matches.size() == 2)
    {
      birth_year = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_issue_year);
    if (matches.size() == 2)
    {
      issue_year = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_expiration_year);
    if (matches.size() == 2)
    {
      expiration_year = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_height);
    if (matches.size() == 2)
    {
      height = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_hair_color);
    if (matches.size() == 2)
    {
      hair_color = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_eye_color);
    if (matches.size() == 2)
    {
      eye_color = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_passport_id);
    if (matches.size() == 2)
    {
      passport_id = matches[1].str();
    }
    else
    {
      valid = false;
    }

    std::regex_match(data, matches, re_country_id);
    if (matches.size() == 2)
    {
      country_id = matches[1].str();
    }
  }

  bool is_valid() const
  {
    return valid;
  }

  bool has_valid_data()
  {
    if (!this->is_valid())
    {
      return false;
    }
    if (!valid_year(birth_year, 1920, 2002))
    {
      return false;
    }
    if (!valid_year(issue_year, 2010, 2020))
    {
      return false;
    }
    if (!valid_year(expiration_year, 2020, 2030))
    {
      return false;
    }
    std::regex_match(height, matches, std::regex{"^(?:.*[^\\d])?(\\d+)(in|cm).*"});
    if (matches.size() != 3)
    {
      return false;
    }
    else
    {
      int height_value = std::stoi(matches[1].str());
      if (matches[2] == "in" && (height_value < 59 || height_value > 77))
      {
        return false;
      }
      if (matches[2] == "cm" && (height_value < 150|| height_value > 193))
      {
        return false;
      }
    }
    if (!std::regex_match(hair_color, std::regex{"#[0-9a-f]{6}"}))
    {
      return false;
    }
    if (!(eye_color == "amb" || eye_color == "blu" || eye_color == "brn" || eye_color == "gry" || eye_color == "grn" || eye_color == "hzl" || eye_color == "oth"))
    {
      return false;
    }
    if (passport_id.size() != 9 || !std::all_of(passport_id.begin(), passport_id.end(), ::isdigit))
    {
      return false;
    }


    return true;
  }
};

std::ostream& operator<<(std::ostream& os, const Passport& passport)
{
  os << "Passport:\n"
     << "Birth Year:      " << passport.birth_year << "\n"
     << "Issue Year:      " << passport.issue_year << "\n"
     << "Expiration Year: " << passport.expiration_year << "\n"
     << "Height:          " << passport.height << "\n"
     << "Hair Color:      " << passport.hair_color << "\n"
     << "Eye Color:       " << passport.eye_color << "\n"
     << "Passport ID:     " << passport.passport_id << "\n"
     << "Country ID:      " << passport.country_id << "\n";

  return os;
}

std::vector<std::string> refine_input(const std::vector<std::string>& input)
{
  std::string passport;
  std::vector<std::string> result;

  for (auto line: input)
  {
    if (line.size() == 0)
    {
      result.push_back(passport);
      passport = "";
    }
    else
    {
      passport += line + " ";
    }
  }
  if (passport.size() > 0)
  {
    result.push_back(passport);
  }

  return result;
}

int part_one(std::vector<std::string> input)
{
  input = refine_input(input);
  int result{0};
  for (auto line: input)
  {
    result += static_cast<int>(Passport(line).is_valid());
  }
  return result;
}

int part_two(std::vector<std::string> input)
{
  input = refine_input(input);
  int result{0};
  for (auto line: input)
  {
    result += static_cast<int>(Passport(line).has_valid_data());
  }
  return result;
}

int main()
{
  utils::Reader reader(std::filesystem::path("../2020/data/input_04.txt"));
  std::vector<std::string> input = reader.get_lines();
  
  std::cout << "The answer to part one is: " << part_one(input) << std::endl;
  
  std::cout << "The answer to part two is: " << part_two(input) << std::endl;
 
  return 0;
}
