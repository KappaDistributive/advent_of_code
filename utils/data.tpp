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

}  // namespace utils