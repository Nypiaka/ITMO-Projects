// :NOTE: хорошее решение, но в бинарных и унарных операциях постоянно повторяются (x, y, z)
//        стоило сделать абстракцию для них, как-то так: (f, a, b) => (...args) => f(a(...args), b(...args))
//        и тогда передавать операции как-то так: binOp((a, b) => a + b, a, b)
const oper = (char, ...args) => {
    switch (char) {
        case '*': return (x, y, z) => args[0](x, y, z) * args[1](x, y, z)
        case '-': return (x, y, z) => args[0](x, y, z) - args[1](x, y, z)
        case '/': return (x, y, z) => args[0](x, y, z) / args[1](x, y, z)
        case '+': return (x, y, z) => args[0](x, y, z) + args[1](x, y, z)
        case 'neg': return (x, y, z) => -args[0](x, y, z)
        case 'sinh': return (x,y,z) => Math.sinh(args[0](x,y,z))
        case 'cosh': return (x,y,z) => Math.cosh(args[0](x,y,z))
        case 'var': return (x,y,z) => {
            switch (args[0]) {
                case 'x': return x
                case 'y' : return y
                case 'z' : return z
            }
        }
    }
}
const add = (f, g) => oper("+", f, g)
const multiply = (f, g) => oper("*", f, g)
const divide = (f, g) => oper("/", f, g)
const subtract = (f, g) => oper("-", f, g)
const negate = (f) => oper("neg", f)
// :NOTE: у константы можно не писать x, y, z - просто _ или ()
const cnst = (f) => (x, y, z) => f
const variable = (s) => oper("var", s)
const pi = cnst(Math.PI)
const e =  cnst(Math.E)
const sinh = (f) => oper("sinh", f)
const cosh = (f) => oper("cosh", f)