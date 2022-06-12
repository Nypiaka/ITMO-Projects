#pragma once

#include "fraction.h"

#include <map>
#include <memory>
#include <ostream>
#include <string>
#include <utility>

struct Expression
{
    virtual ~Expression() = default;
    virtual Fraction eval(std::map<std::string, Fraction> const & values = {}) const = 0;

    virtual Expression * clone() const = 0;

    friend std::ostream & operator<<(std::ostream & out, const Expression & expression)
    {
        return out << expression.to_string();
    };

    virtual std::string to_string() const = 0;
};

struct AbstractBinaryOperation : public Expression
{
public:
    AbstractBinaryOperation(const Expression & left, const Expression & right)
        : l(left.clone())
        , r(right.clone())
    {
    }
    Fraction eval(std::map<std::string, Fraction> const & values = {}) const override;
    std::string to_string() const override;

protected:
    virtual std::string get_operator() const = 0;
    virtual Fraction evaluate(const Fraction & a, const Fraction & b) const = 0;
    std::shared_ptr<Expression> l;
    std::shared_ptr<Expression> r;
};
struct AbstractUnaryOperation : public Expression
{
public:
    AbstractUnaryOperation(const Expression & alone)
        : a(alone.clone())
    {
    }
    Fraction eval(std::map<std::string, Fraction> const & values = {}) const override;
    std::string to_string() const override;

protected:
    virtual std::string get_operator() const = 0;
    virtual Fraction evaluate(const Fraction & a) const = 0;
    std::shared_ptr<Expression> a;
};
struct Add : public AbstractBinaryOperation
{
public:
    Add(const Expression & left, const Expression & right)
        : AbstractBinaryOperation(left, right){};
    Expression * clone() const override;

protected:
    Fraction evaluate(const Fraction & left, const Fraction & right) const override;
    std::string get_operator() const override;
};
struct Multiply : public AbstractBinaryOperation
{
public:
    Multiply(const Expression & left, const Expression & right)
        : AbstractBinaryOperation(left, right){};
    Expression * clone() const override;

protected:
    Fraction evaluate(const Fraction & left, const Fraction & right) const override;
    std::string get_operator() const override;
};
struct Divide : public AbstractBinaryOperation
{
public:
    Divide(const Expression & left, const Expression & right)
        : AbstractBinaryOperation(left, right){};
    Expression * clone() const override;

protected:
    Fraction evaluate(const Fraction & left, const Fraction & right) const override;
    std::string get_operator() const override;
};
struct Subtract : public AbstractBinaryOperation
{
public:
    Subtract(const Expression & left, const Expression & right)
        : AbstractBinaryOperation(left, right){};
    Expression * clone() const override;

protected:
    Fraction evaluate(const Fraction & left, const Fraction & right) const override;
    std::string get_operator() const override;
};
struct Negate : public AbstractUnaryOperation
{
public:
    Negate(const Expression & alone)
        : AbstractUnaryOperation(alone)
    {
    }
    Expression * clone() const override;

protected:
    Fraction evaluate(const Fraction & alone) const override;
    std::string get_operator() const override;
};
struct Const : public Expression
{
public:
    Const(const Fraction alone)
        : a(alone)
    {
    }
    Fraction eval([[maybe_unused]] std::map<std::string, Fraction> const & values = {}) const override;
    Expression * clone() const override;
    std::string to_string() const override;

private:
    const Fraction a;
};
struct Variable : public Expression
{
public:
    Variable(const std::string & alone)
        : a(alone)
    {
    }
    Fraction eval(std::map<std::string, Fraction> const & values = {}) const override;
    Expression * clone() const override;
    std::string to_string() const override;

private:
    const std::string a;
};
Add operator+(const Expression & left, const Expression & right);
Multiply operator*(const Expression & left, const Expression & right);
Divide operator/(const Expression & left, const Expression & right);
Subtract operator-(const Expression & left, const Expression & right);
Negate operator-(const Expression & left);