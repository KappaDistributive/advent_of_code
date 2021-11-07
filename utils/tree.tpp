namespace utils {
namespace tree {

template<typename T>
Node<T>::Node(const T& data)
  : m_data(data) {
}

template<typename T>
const T& Node<T>::getData() const {
  return m_data;
}

template<typename T>
T& Node<T>::getData() {
  return m_data;
}

template<typename T>
void Node<T>::setData(const T& data) {
  m_data = data;
}

template<typename T>
Node<T>* Node<T>::addChild(const Node<T>& node) {
  m_children.emplace_back(node);

  return &m_children.back();
}

template<typename T>
Node<T>& Node<T>::getChild(const size_t& childIndex) {
  auto it = m_children.begin();
  std::advance(it, childIndex);
  return *it;
}

template<typename T>
std::vector<Node<T>*> Node<T>::getChildren() {
  std::vector<Node<T>*> children;
  for (auto it = m_children.begin(); it != m_children.end(); it++) {
    children.push_back(&*it);
  }

  return children;
}

template<typename T>
size_t Node<T>::size() const {
  size_t size{1};
  for (auto child: m_children) {
    size += child.size();
  }

  return size;
}

template<typename T>
bool Node<T>::operator==(const Node<T>& other) const {
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
bool Node<T>::operator!=(const Node<T>& other) const {
  return !(*this == other);
}

template<typename T>
std::ostream& operator<<(std::ostream& os, const Node<T>& node) {
  os << "Node<data: " << node.getData()
     << "; #children: " << node.m_children.size() << ">";

  return os;
}

template<typename T>
typename Node<T>::template Iterator<Node<T>>
Node<T>::begin() {
  return Iterator<Node<T>>(this);
}

template<typename T>
typename Node<T>::template Iterator<Node<T>>
Node<T>::end() {
  return Iterator<Node<T>>(nullptr);
}

template<typename T>
template<typename NodeType>
Node<T>::Iterator<NodeType>::Iterator(NodeType* node)
  : m_node(node) {
  }

template<typename T>
template<typename NodeType>
typename Node<T>::template Iterator<NodeType>&
Node<T>::Iterator<NodeType>::operator++() {
  // perform depth-first search
  const size_t numChildren = this->m_node->m_children.size();
    if(numChildren > 0) {
      if(numChildren > 1) {
        for(auto it = this->m_node->m_children.rbegin();
            it != std::prev(this->m_node->m_children.rend(), 1);
            it++) {
          m_memory.push(&(*it));
        }
      }
      this->m_node = &this->m_node->m_children.front();
    } else if(m_memory.size() > 0) {
      this->m_node = m_memory.top();
      m_memory.pop();
    } else {
      this->m_node = nullptr;
    }
    return *this;
}

template<typename T>
template<typename NodeType>
typename Node<T>::template Iterator<NodeType>
Node<T>::Iterator<NodeType>::operator++(int) {
  auto it = *this;
  ++(*this);
  return it;
}

template<typename T>
template<typename NodeType>
bool
Node<T>::Iterator<NodeType>::operator==(const Node<T>::Iterator<NodeType>& other) const {
  bool temp = this->m_node == other.m_node;
  return temp;
}

template<typename T>
template<typename NodeType>
bool
Node<T>::Iterator<NodeType>::operator!=(const Node<T>::Iterator<NodeType>& other) const {
  return !(*this == other);
}

template<typename T>
template<typename NodeType>
NodeType*
Node<T>::Iterator<NodeType>::operator*() const {
  return this->m_node;
}

template<typename Node>
Tree<Node>::Tree(const Node& root)
  : m_root(root) {
  }

template<typename Node>
Node&
Tree<Node>::getRoot() {
  return m_root;
}

template<typename Node>
std::vector<Node*>
Tree<Node>::findByData(const Node& reference) {
  std::vector<Node*> matches;

  for (auto it = m_root.begin(); it != m_root.end(); it++) {
    if ((*it)->getData() == reference.getData()) {
      Node* node = *it;
      matches.push_back(node);
    }
  }

  return matches;
}

template<typename Node>
bool
Tree<Node>::operator==(const Tree<Node>& other) const {
  return this->m_root == other.m_root;
}

template<typename Node>
bool
Tree<Node>::operator!=(const Tree<Node>& other) const {
  return !(*this == other);
}

}  // namespace tree
}  // namespace utils
