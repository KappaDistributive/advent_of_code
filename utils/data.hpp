#pragma once

#include <iostream>
#include <vector>

namespace utils {

template<typename T>
class Node {
 private:
  T m_data;
  std::vector<Node<T>> m_children;

 public:
  explicit Node(const T& data);

  const T& getData() const;

  void setData(const T& data);

  size_t addChild(const Node child);

  Node getChild(const size_t& childIndex) const;

  // friend std::ostream& operator<<(std::ostream& os, const Node& node);
};

// template <typename T>
// class Tree {
//  private:
//   Node<T>& root_;
//
//  public:
//   Node<T>& root();
//
//   Node<T>& insert();
//
//   Node<T>* follow_path(const std::vector<size_t>& path);
// };

}  // namespace utils

#include "data.tpp"