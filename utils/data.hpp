#pragma once

#include <deque>
#include <list>
#include <iostream>
#include <iterator>
#include <vector>

namespace utils {

template<typename T>
class Node {
 private:
  T m_data;
  std::list<Node> m_children;

 public:
  explicit Node(const T& data);

  const T& getData() const;

  void setData(const T& data);

  Node& addChild(const Node& child);

  Node& getChild(const size_t& childIndex);

  bool operator==(const Node& other) const;

  bool operator!=(const Node& other) const;

  friend std::ostream& operator<<(std::ostream& os, const Node& node) {
    os << "Node<data: " << node.getData()
       << "; #children: " << node.m_children.size() << ">";

    return os;
  }
};


template <typename T>
class Tree {
 private:
  Node<T> m_root;

 public:
  explicit Tree(const Node<T>& root);

  Node<T>& getRoot();
  bool operator==(const Tree& other) const;

  bool operator!=(const Tree& other) const;
};

}  // namespace utils

#include "data.tpp"
