#include "data.hpp"

namespace utils {

template<typename T>
Node<T>::Node(const T& data)
  : m_data(data) {
}

template<typename T>
const T& Node<T>::getData() const {
  return m_data;
}

template<typename T>
void Node<T>::setData(const T& data) {
  m_data = data;
}

template<typename T>
Node<T>& Node<T>::addChild(const Node& node) {
  m_children.emplace_back(node);

  return m_children.back();
}

template<typename T>
Node<T>& Node<T>::getChild(const size_t& childIndex) {
  auto it = m_children.begin();
  std::advance(it, childIndex);
  return *it;
}

template<typename T>
bool Node<T>::operator==(const Node& other) const {
  bool equal{true};
  if (this->getData() != other.getData()) {
    equal = false;
  } else if (this->m_children.size() != other.m_children.size()) {
    equal = false;
  } else {
    auto it_lhs = this->m_children.begin();
    auto it_rhs = other.m_children.begin();
    for (size_t childIndex{0}; childIndex < this->m_children.size(); childIndex++) {
      if (*it_lhs != *it_rhs) {
        equal = false;
        break;
      }
      it_lhs++;
      it_rhs++;
    }
  }

  return equal;
}

template<typename T>
bool Node<T>::operator!=(const Node& other) const {
  return !(*this == other);
}

template<typename T>
Tree<T>::Tree(const Node<T>& root)
  : m_root(root) {
  }

template<typename T>
Node<T>& Tree<T>::getRoot() {
  return m_root;
}

template<typename T>
bool Tree<T>::operator==(const Tree& other) const {
  return this->m_root == other.m_root;
}

template<typename T>
bool Tree<T>::operator!=(const Tree& other) const {
  return !(*this == other);
}

}  // namespace utils