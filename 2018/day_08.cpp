#include <algorithm>
#include <numeric>

#include "../utils/input.hpp"


std::tuple<size_t, size_t, std::vector<size_t>>
parse_data(const std::vector<size_t>& data) {
    size_t num_children = data[0];
    size_t num_meta = data[1];
    std::vector<size_t> tail
      = std::vector<size_t>(data.begin() + 2, data.end());
    std::vector<size_t> scores;
    size_t meta_sum{0};

    for (size_t child_index{0}; child_index < num_children; child_index++) {
        auto update = parse_data(tail);
        meta_sum += std::get<0>(update);
        scores.push_back(std::get<1>(update));
        tail = std::get<2>(update);
    }

    meta_sum += std::accumulate(tail.begin(), tail.begin() + num_meta, 0);

    int score{0};
    if (num_children == 0) {
        score = std::accumulate(tail.begin(), tail.begin() + num_meta, 0);
    } else {
        for (size_t meta_index{0}; meta_index < num_meta; meta_index++) {
            auto meta_entry = tail[meta_index];
            if (meta_entry > 0 && meta_entry <= scores.size()) {
                score += scores[meta_entry - 1];
            }
        }
    }

    tail = std::vector<size_t>(tail.begin() + num_meta, tail.end());
    return {meta_sum, score, tail};
}


int part_one(const std::vector<size_t>& input) {
    auto [meta_sum, score, _] = parse_data(input);
    return meta_sum;
}


int part_two(const std::vector<size_t>& input) {
    auto [meta_sum, score, _] = parse_data(input);
    return score;
}


int main() {
  utils::Reader reader(std::filesystem::path("../2018/data/input_08.txt"));
  auto input = utils::split_string(reader.get_lines()[0], ' ');
  std::vector<size_t> transformed_input;
  std::transform(input.begin(),
                 input.end(),
                 std::back_insert_iterator(transformed_input),
                 [] (const std::string& entry) { return std::stoul(entry); });

  auto answer_one =  part_one(transformed_input);
  std::cout << "The answer to part one is: " << answer_one << std::endl;
  auto answer_two =  part_two(transformed_input);
  std::cout << "The answer to part two is: " << answer_two << std::endl;

  return 0;
}
