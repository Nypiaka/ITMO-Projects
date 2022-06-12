#include <AVLTree.h>
#include <cstdlib>
#include <iostream>

class AVLTree::Node
{
public:
    Node(int value)
        : value(value)
    {
    }
    int height = 0;
    int value;
    Node * left = nullptr;
    Node * right = nullptr;
    void setHeight()
    {
        height = std::max(getHeight(left), getHeight(right)) + 1;
    }
    ~Node()
    {
        delete left;
        delete right;
    }
};

bool AVLTree::empty() const
{
    return sizeOfTree == 0;
}
std::size_t AVLTree::size() const
{
    return sizeOfTree;
}
int AVLTree::getHeight(Node * current)
{
    return current == nullptr ? 0 : current->height;
}

AVLTree::Node * AVLTree::rotateRight(Node * current)
{
    Node * leftChild = current->left;
    Node * rightChildOfLeftChild = leftChild->right;
    leftChild->right = current;
    current->left = rightChildOfLeftChild;
    current->setHeight();
    leftChild->setHeight();
    return leftChild;
}

AVLTree::Node * AVLTree::rotateLeft(Node * current)
{
    Node * rightChild = current->right;
    Node * leftChildOfRightChild = rightChild->left;
    rightChild->left = current;
    current->right = leftChildOfRightChild;
    current->setHeight();
    rightChild->setHeight();
    return rightChild;
}

int AVLTree::getDifference(Node * current)
{
    if (current == nullptr)
        return 0;
    return getHeight(current->left) - getHeight(current->right);
}

AVLTree::Node * AVLTree::insertRecursive(Node * current, int value)
{

    if (current == nullptr) {
        sizeOfTree++;
        current = new Node(value);
        return current;
    }
    if (value == current->value) {
        return current;
    }
    if (value < current->value)
        current->left = insertRecursive(current->left, value);
    else
        current->right = insertRecursive(current->right, value);
    return reBalance(current, current->left != nullptr && value < current->left->value, current->right != nullptr && value < current->right->value);
}
AVLTree::Node * AVLTree::reBalance(Node * current, bool leftArgument, bool rightArgument)
{
    current->setHeight();
    int difference = getDifference(current);
    if (difference > 1 && leftArgument)
        return rotateRight(current);
    if (difference < -1 && !rightArgument)
        return rotateLeft(current);
    if (difference > 1 && !leftArgument) {
        return bigRotateRight(current);
    }
    if (difference < -1 && rightArgument) {
        return bigRotateLeft(current);
    }
    return current;
}
bool AVLTree::insert(int value)
{
    if (contains(value)) {
        return false;
    }
    head = insertRecursive(head, value);
    return true;
}
std::vector<int> AVLTree::values() const
{
    std::vector<int> container;
    container.reserve(sizeOfTree);
    getValues(head, container);
    return container;
}
void AVLTree::getValues(Node * current, std::vector<int> & container) const
{
    if (current != nullptr) {
        getValues(current->left, container);
        container.push_back(current->value);
        getValues(current->right, container);
    }
}

AVLTree::Node * AVLTree::removeRecursive(Node * current, int value)
{
    if (value < current->value)
        current->left = removeRecursive(current->left, value);
    else if (value > current->value)
        current->right = removeRecursive(current->right, value);
    else if ((current->left == nullptr) ||
             (current->right == nullptr)) {
        Node * node;
        if (current->left != nullptr) {
            node = current->left;
        }
        else
            node = current->right;
        if (node == nullptr) {
            node = current;
            current = nullptr;
        }
        else
            *current = *node;
        node->left = nullptr;
        node->right = nullptr;
        delete node;
        return current;
    }
    else {
        Node * node = current->right;
        while (node->left != nullptr) {
            node = node->left;
        }
        current->value = node->value;
        current->right = removeRecursive(current->right, node->value);
    }
    return reBalance(current, getDifference(current->left) >= 0, getDifference(current->right) > 0);
}
AVLTree::Node * AVLTree::bigRotateRight(Node * node)
{
    node->left = rotateLeft(node->left);
    return rotateRight(node);
}
AVLTree::Node * AVLTree::bigRotateLeft(Node * node)
{
    node->right = rotateRight(node->right);
    return rotateLeft(node);
}
bool AVLTree::remove(int value)
{
    if (!contains(value)) {
        return false;
    }
    head = removeRecursive(head, value);
    sizeOfTree--;
    return true;
}

bool AVLTree::contains(int value) const
{
    Node * node = head;
    while (node != nullptr) {
        if (value > node->value) {
            node = node->right;
        }
        else if (value < node->value) {
            node = node->left;
        }
        else {
            return true;
        }
    }
    return false;
}

AVLTree::~AVLTree()
{
    delete head;
}