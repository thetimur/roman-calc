#include <iostream>
#include <string>
#include <vector>
#include <algorithm>
#include <unordered_map>
#include <cctype>

// Functor used to encrypt pairs.
struct pair_hash {
    template <class T1, class T2>
    std::size_t operator() (const std::pair<T1,T2> &p) const {
        auto h1 = std::hash<T1>{}(p.first);
        auto h2 = std::hash<T2>{}(p.second);

        return h1 ^ h2;  
    }
};

/**
 * @class RomanConverter
 * The class is used to convert numbers to and from the Roman numeral system.
 * 
 * Methods:
 * int64_t to_int64(const std::string &value) // converts to integer value from Roman value represented as string.
 * std::string to_roman(int64_t value) // converts to Roman numver from integer value. 
 */
class RomanConverter {
private:
    const int BOUND = 3999;
    std::unordered_map<char, int64_t> rule_add = {
        {'I', 1},
        {'V', 5},
        {'X', 10},
        {'L', 50},
        {'C', 100},
        {'D', 500},
        {'M', 1000}
    };

    std::unordered_map<std::pair<char, char>, int64_t, pair_hash> rule_div = {
        {{'I', 'V'}, 3},
        {{'I', 'X'}, 8},
        {{'X', 'L'}, 30},
        {{'X', 'C'}, 80},
        {{'C', 'D'}, 300},
        {{'C', 'M'}, 800}
    };

    std::vector<std::pair<int, std::string> > weight = {
        {1000, "M"},
        {900, "CM"},
        {500, "D"},
        {400, "CD"},
        {100, "C"},
        {90, "XC"},
        {50, "L"},
        {40,"XL"},
        {10, "X"},
        {9, "IX"},
        {5, "V"},
        {4, "IV"},
        {1, "I"},
        {0, "Z"},
    };
public:
    int64_t to_int64(const std::string &value) {
        if (value.size() == 1 && value[0] == 'Z') {
            return 0;
        }
        int64_t result = 0;
        char prev_literal = 0;

        for (char literal : value) {
            if (prev_literal && rule_add[prev_literal] < rule_add[literal]) {
                result += rule_div[std::make_pair(prev_literal, literal)];
            } else {
                result += rule_add[literal];
            }
            prev_literal = literal;
        }

        return result;
    }

    std::string to_roman(int64_t value) {
        if (!value) {
            return "Z";
        }
        if (std::abs(value) > BOUND) {
            throw std::logic_error("Roman number overflow");
        }
        std::string result = "";

        if (value < 0) {
            result = "-";
            value = abs(value);
        }

        while (value > 0) {
            for (auto w : weight) {
                if (value >= w.first) {
                    value -= w.first;
                    result += w.second;
                    break;
                }
            }
        }
        return result;
    }
};

/**
 * @class ElementType
 * The class lists all the possible elements of our expressions.
 */
enum class ElementType {
    BRACKET,
    BINARY_OPERATION,
    VALUE
};

/**
 * @class Element
 * Class describing the elements of an arithmetic expression
 * 
 * Methods:
 * Element(int64_t value) // constructor for value-type element.
 * Element(int value, ElementType type) // constructor for brackets and binary operators.
 * ElementType label() // returns current element's type.
 * int priority() // returnst current element's priority.
 * Element* proceed(Element* left, Element* right) // proceeds a given operation for two values. Uses only for binary operations.
 * int64_t value() // returns current elemnt's value.
 */
class Element {
public:
    Element(int64_t value) : value_(value), label_(ElementType::VALUE), priority_(3) {}

    Element(int value, ElementType type) : value_(value), label_(type) {
        switch (value) {
            case '+':
            case '-':
                priority_ = 0;
                break;

            case '*':
            case '/':
                priority_ = 1;
                break;

            case '(':
            case ')':
                priority_ = -1;
                break;
            
            default:
                break;
        }
    }

    ElementType label() const {
        return label_;
    }

    int priority() const {
        return priority_;
    }

    int divide(int left, int right) const {
        if ((left <= 0 && right < 0) || (left >= 0 && right > 0)) {
            return left / right;
        } else {
            return -(abs(left) + abs(right) - 1) / abs(right);
        }
    }

    Element* proceed(Element* left, Element* right) const {
        int64_t result = 0;

        switch (value_) {
        case '+':
            result = left->value() + right->value();
            break;
        case '-':
            result = left->value() - right->value();
            break;
        case '*':
            result = left->value() * right->value();
            break;
        case '/':
            if (right->value() == 0) {
                throw std::logic_error("Division by zero");
            }
            result = divide(left->value(), right->value());
            break;
        default:
            break;
        }

        return new Element(result);
    }

    int64_t value() const {
        return value_;
    }

private:
    int64_t value_;
    ElementType label_;
    int priority_;
};

/**
 * @class Expression solver
 * Parses and solves an arithmetic expression.
 * 
 * Methods:
 * bool is_roman(const char c) // checks that the current character is a Roman numeral.
 * bool is_operation(const char c) //  checks that the current character is a binary operation.
 * bool is_unary(int current) // checks than an element on a current position can ba an unary minus.
 * int64_t read_number() // reads roman number starting from the current solver's state.
 * ExpressionSolver(std::string expression) // parses given string to a Reverse Polish notation.
 * std::string solve() // solves an expression from a current solver's state.
 */
class ExpressionSolver {
private:
    std::string data_; // input string for solver.
    int position_; // current solver's state.
    const std::vector<char> available_symbols = {'I', 'V', 'X', 'L', 'C', 'D', 'M', 'Z'};
    RomanConverter converter;
    std::vector<Element*> stack, out; // out -- Reverse Polish notation representarion array. stack -- some helpful array.

    bool is_roman(const char c) const {
        return std::find(available_symbols.begin(), available_symbols.end(), c) != available_symbols.end();
    }

    bool is_operation(const char c) const {
        return c == '+' || c == '-' || c == '*' || c == '/';
    }

    bool is_unary(int current) const {
        return (current + 1 < (int)data_.size() && data_[current + 1] == '(' ||
                is_roman(data_[current + 1])) && 
                data_[current] == '-' && 
                (!current || (current > 0 && 
                (is_operation(data_[current - 1]) || data_[current - 1] == ')')));
    }

    int64_t read_number() {
        std::string result = "";

        while (position_ < (int)data_.size() && is_roman(data_[position_])) {
            result += data_[position_++];
        }
        return converter.to_int64(result);
    }
public:
    ExpressionSolver(std::string expression) : data_(expression), position_(0) {
        data_.erase(std::remove_if(data_.begin(), data_.end(), [](unsigned char x) { return std::isspace(x); }), data_.end());
        int unarity = 1;
        while (position_ < (int)data_.size()) {
            if (is_roman(data_[position_])) {
                int64_t value = read_number();
                out.push_back(new Element(value * unarity));
                position_--;
            } else if (data_[position_] == '(') {
                stack.push_back(new Element('(', ElementType::BRACKET));
            } else if (data_[position_] == ')') {
                while (!stack.empty() && stack.back()->label() != ElementType::BRACKET) {
                    out.push_back(stack.back());
                    stack.pop_back();
                }
                if (stack.empty()) {
                    throw std::logic_error("Invalid bracket sequence in expression");
                }

                delete stack.back();
                stack.pop_back();
            } else if (is_operation(data_[position_])) {
                if (is_unary(position_)) {
                    unarity = -1;
                    position_++;
                    continue;
                }

                Element* current = new Element(data_[position_], ElementType::BINARY_OPERATION);
                while (!stack.empty() && stack.back()->priority() >= current->priority()) {
                    out.push_back(stack.back());
                    stack.pop_back();
                }
                stack.push_back(current);
            } else {
                throw std::logic_error("Bad symbol on position " + std::to_string(position_ + 1));
            }
            unarity = 1;
            position_++;
        }

        while (!stack.empty()) {
            if (stack.back()->label() != ElementType::BINARY_OPERATION) {
                throw std::logic_error("Invalid bracket sequence in expression");
            }
            out.push_back(stack.back());
            stack.pop_back();
        }
    }
    
    std::string solve() {
        if (out.empty()) {
            return "Z";
        }

        for (auto element : out) {
            if (element->label() == ElementType::BINARY_OPERATION) {
                if (stack.size() < 2) {
                    throw std::logic_error("Invalid expression format");
                }
                auto right = stack.back();
                stack.pop_back();
                auto left = stack.back();
                stack.pop_back();

                if (left->label() != ElementType::VALUE || right->label() != ElementType::VALUE) {
                    throw std::logic_error("Invalid expression format");
                }
                try {
                    stack.push_back(element->proceed(left, right));
                } catch (std::logic_error &e) {
                    delete element;
                    throw e;
                }
                delete left;
                delete right;
                delete element;
            } else {
                stack.push_back(element);
            }
        }

        if (stack[0]->label() != ElementType::VALUE) {
            throw std::logic_error("Invalid expression format");
        }
        try {
            auto result = converter.to_roman(stack[0]->value());
            delete stack[0];
            return result;
        } catch (std::logic_error &e) {
            throw e;
        }
    }

};

int main() {
    std::string s;
    while (std::getline(std::cin, s)) {
        try {
            ExpressionSolver solver(s);
            std::cout << solver.solve() << std::endl;
        } catch (std::logic_error &e) {
            std::cout << "error: " << e.what() << std::endl;
        }
    }
    return 0;
}