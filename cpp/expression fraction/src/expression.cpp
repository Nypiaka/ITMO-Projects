#include "expression.h"

Fraction AbstractBinaryOperation::eval(const std::map<std::string, Fraction> & values) const
{
    return evaluate(l->eval(values), r->eval(values));
}
std::string AbstractBinaryOperation::to_string() const
{
    return "(" + l->to_string() + get_operator() + r->to_string() + ")";
}
Fraction AbstractUnaryOperation::eval(std::map<std::string, Fraction> const & values) const
{
    return evaluate(a->eval(values));
}
std::string AbstractUnaryOperation::to_string() const
{
    return get_operator() + "(" + a->to_string() + ")";
}
Fraction Add::evaluate(const Fraction & left, const Fraction & right) const
{
    return left + right;
}
std::string Add::get_operator() const
{
    return " + ";
}
Expression * Add::clone() const
{
    return new Add(*this);
}
Fraction Multiply::evaluate(const Fraction & left, const Fraction & right) const
{
    return left * right;
}
std::string Multiply::get_operator() const
{
    return " * ";
}
Expression * Multiply::clone() const
{
    return new Multiply(*this);
}
Fraction Subtract::evaluate(const Fraction & left, const Fraction & right) const
{
    return left - right;
}
std::string Subtract::get_operator() const
{
    return " - ";
}
Expression * Subtract::clone() const
{
    return new Subtract(*this);
}
Fraction Divide::evaluate(const Fraction & left, const Fraction & right) const
{
    return left / right;
}
std::string Divide::get_operator() const
{
    return " / ";
}
Expression * Divide::clone() const
{
    return new Divide(*this);
}
Fraction Negate::evaluate(const Fraction & alone) const
{
    return -alone;
}
std::string Negate::get_operator() const
{
    return "-";
}
Expression * Negate::clone() const
{
    return new Negate(*this);
}
Fraction Const::eval([[maybe_unused]] const std::map<std::string, Fraction> & values) const
{
    return a;
}
std::string Const::to_string() const
{
    return std::to_string(a.numerator()) + "/" + std::to_string(a.denominator());
}
Expression * Const::clone() const
{
    return new Const(*this);
}
Fraction Variable::eval([[maybe_unused]] const std::map<std::string, Fraction> & values) const
{
    return values.at(a);
}
std::string Variable::to_string() const
{
    return a;
}
Expression * Variable::clone() const
{
    return new Variable(*this);
}
Add operator+(const Expression & left, const Expression & right)
{
    return Add(left, right);
}
Multiply operator*(const Expression & left, const Expression & right)
{
    return Multiply(left, right);
}
Divide operator/(const Expression & left, const Expression & right)
{
    return Divide(left, right);
}
Subtract operator-(const Expression & left, const Expression & right)
{
    return Subtract(left, right);
}
Negate operator-(const Expression & left)
{
    return Negate(left);
}