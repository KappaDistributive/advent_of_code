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
size_t Node<T>::addChild(Node node) {
  size_t index = m_children.size();
  m_children.push_back(node);

  return index;
}

template<typename T>
Node<T> Node<T>::getChild(const size_t& childIndex) const {
  return m_children[childIndex];
}

template<typename T>
bool Node<T>::operator==(const Node& other) const {
  bool equal{true};
  if (this->getData() != other.getData()) {
    equal = false;
  } else if (this->m_children.size() != other.m_children.size()) {
    equal = false;
  } else {
    for (size_t childIndex{0}; childIndex < this->m_children.size(); childIndex++) {
      if (!(this->m_children[childIndex] == other.m_children[childIndex])) {
        equal = false;
        break;
      }
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
const Node<T>& Tree<T>::getRoot() const {
  return m_root;
}

template<typename T>
bool Tree<T>::operator==(const Tree& other) const {
  return this->getRoot() == other.getRoot();
}

template<typename T>
bool Tree<T>::operator!=(const Tree& other) const {
  return !(*this == other);
}

}  // namespace utils