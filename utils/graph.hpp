#pragma once

#include <iostream>
#include <map>
#include <vector>

namespace utils {
namespace graph {

template <typename T>
class Node {
 private:
  T m_data;

 public:
  explicit Node(const T& data);

  explicit Node(const Node<T>& node);

  bool operator==(const Node<T>& other) const;

  bool operator!=(const Node<T>& other) const;

  template<typename S>
  friend std::ostream& operator<<(std::ostream& os, const Node<S>& node);
};

template<typename NodeType, bool directed>
class Graph {
};

}  // namespace graph
}  // namespace utils

#include "graph.tpp"
