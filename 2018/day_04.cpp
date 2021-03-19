#include <array>
#include <cassert>
#include <map>
#include <regex>
#include <set>

#include "../utils/input.hpp"


std::vector<std::string> sort_input(const std::vector<std::string>& input) {
    auto result = input;

    std::sort(
            result.begin(),
            result.end(),
            [] (std::string lhs, std::string rhs) -> bool {
                return lhs.substr(0, 18) < rhs.substr(0, 18);
            }
    );

    return result;
}

std::map<int, std::set<std::string>> calculate_sleeping_patterns(const std::vector<std::string>& input) {
    auto sorted_input = sort_input(input);
    int active_guard_id{-1};
    bool guard_sleeps{false};
    std::string bedtime;
    std::map<int, std::set<std::string>> sleeping_patterns;

    std::regex guard_re{"^.*#(\\d+).*$"};
    std::smatch matches;

    for (auto line: sorted_input) {
        std::regex_match(line, matches, guard_re);
        if (matches.size() == 2) {
            active_guard_id = std::stoi(matches[1]);
            continue;
        }

        if (line.find("falls asleep") != std::string::npos) {
            guard_sleeps = true;
            bedtime = line.substr(0, 18);

        } else {
            assert(line.find("wakes up") != std::string::npos);
            sleeping_patterns.emplace(std::make_pair(active_guard_id, std::set<std::string>()));
            guard_sleeps = false;
            auto time = line.substr(0, 18);

            int bedtime_minute = std::stoi(bedtime.substr(15, 2));
            int wakeup_minute = std::stoi(time.substr(15, 2));

            for (int minute{bedtime_minute}; minute < wakeup_minute; minute++) {
                std::string sleeping_minute = line.substr(0, 15);
                if (minute < 10) {
                    sleeping_minute += "0";
                }
                sleeping_minute += std::to_string(minute) + "]";
                sleeping_patterns.at(active_guard_id).insert(sleeping_minute);
            }
        }
    }

    return sleeping_patterns;
}

size_t part_one(const std::vector<std::string>& input) {
    auto sleeping_patterns = calculate_sleeping_patterns(input);
    int sleepy_guard_id{-1};
    size_t sleepy_guard_score{0};

    for (auto [guard_id, sleeping_pattern]: sleeping_patterns) {
        if (sleepy_guard_id == -1 || sleeping_pattern.size() > sleepy_guard_score) {
            sleepy_guard_id = guard_id;
            sleepy_guard_score = sleeping_pattern.size();
        }
    }

    auto sleeping_pattern = sleeping_patterns.at(sleepy_guard_id);
    std::array<int, 60> sleeping_minutes{0};
    for (auto time: sleeping_pattern) {
        sleeping_minutes[std::stoi(time.substr(15, 2))]++;
    }
    size_t sleepy_minute_index{0};
    int sleepy_minute_duration{0};
    for (size_t index{1};  index < sleeping_minutes.size(); index++) {
        if (sleeping_minutes[index] > sleepy_minute_duration) {
            sleepy_minute_index = index;
            sleepy_minute_duration = sleeping_minutes[index];
        }
    }
    return sleepy_guard_id * sleepy_minute_index;
}

size_t part_two(const std::vector<std::string>& input) {
    auto sleeping_patterns = calculate_sleeping_patterns(input);
    int guard_id{-1};
    int guard_score{-1};
    int sleepy_minute{-1};

    for (auto [id, sleeping_pattern]: sleeping_patterns) {
        std::array<int, 60> sleeping_minutes{0};
        for (auto time: sleeping_pattern) {
            sleeping_minutes[std::stoi(time.substr(15, 2))]++;
        }

        for (size_t index{0}; index< sleeping_minutes.size(); index++) {
            if (sleeping_minutes[index] > guard_score) {
                sleepy_minute = index;
                guard_id = id;
                guard_score = sleeping_minutes[index];
            }
        }
    }
    return guard_id * sleepy_minute;
}



int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_04.txt"));
  auto input = reader.get_lines();

  auto answer_one =  part_one(input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;
  return 0;
}

