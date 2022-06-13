class OperPrototype {
    constructor(...args) {
        this.args = args
    }

    type;
    countOfArgs;
    prefix = function () {
        // if (this.type === 'bin') {
        //     return '(' + this.opSymb + ' ' + this.args[0].prefix() + ' ' + this.args[1].prefix() + ')'
        // }
        if (this.type === 'mono') {
            return String(this.args[0])
        }
            // if (this.type === 'un') {
            //     return '(' + this.opSymb + ' ' + this.args[0].prefix() + ')'
        // }
        else {
            let constructor = '(' + this.opSymb
            for (let i = 0; i < this.args.length; i++) {
                constructor += ' '
                constructor += this.args[i].prefix()
            }
            return constructor + ')'
        }

    }
    toString = function () {
        if (this.type === 'bin') {
            return this.args[0].toString() + ' ' + this.args[1].toString() + ' ' + this.opSymb
        }
        if (this.type === 'mono') {
            return String(this.args[0])
        }
        if (this.type === 'un') {
            return this.args[0].toString() + ' ' + this.opSymb
        }
    }
    evaluate = function (x, y, z) {
        let args1 = {};
        for (let i in this.args) {
            args1[i] = this.args[i]
        }
        for (let i = 0; i < this.args.length; i++) {
            if (typeof this.args[i] !== 'number' && typeof this.args[i] !== 'string') {
                this.args[i] = this.args[i].evaluate(x, y, z);
            }
        }
        if (this.type === 'mono') {
            if (typeof this.args[0] === 'number')
                return this.args[0]
            else {
                switch (this.args[0].trim()) {
                    case 'x':
                        return x
                    case 'y':
                        return y
                    case 'z':
                        return z
                }
            }
        }
        let args2 = [];
        for (let i in this.args) {
            args2[i] = this.args[i]
        }
        for (let i in this.args) {
            this.args[i] = args1[i]
        }
        return this.executor(...args2);
    }
}

function classMaker(...args) {
    class Current extends OperPrototype {
        executor = args[0]
        type = args[1]
        opSymb = args[2]
        countOfArgs = args[3]
    }

    return Current
}

let Add = classMaker((...args) => args[0] + args[1], 'bin', '+', 2)
let Const = classMaker((a) => a, 'mono')
let Subtract = classMaker((...args) => args[0] - args[1], 'bin', '-', 2)
let Negate = classMaker((...args) => -args[0], 'un', 'negate', 1)
let Sinh = classMaker((...args) => Math.sinh(args[0]), 'un', 'sinh', 1)
let Cosh = classMaker((...args) => Math.cosh(args[0]), 'un', 'cosh', 1)
let Divide = classMaker((...args) => args[0] / args[1], 'bin', '/', 2)
let Multiply = classMaker((...args) => args[0] * args[1], 'bin', '*', 2)
let Variable = classMaker((...args) => args[0], 'mono')
let Mean = classMaker(function (...args) {
    let sum = 0;
    for (let i = 0; i < args.length; i++) {
        sum += args[i]
    }
    return sum / args.length
}, 'unlim', 'mean', Infinity)
let Var = classMaker(function (...args) {
    let sum = 0;
    for (let i = 0; i < args.length; i++) {
        sum += args[i]
    }
    let sred = sum / args.length
    let disp = 0;
    for (let i = 0; i < args.length; i++) {
        disp += (args[i] - sred) * (args[i] - sred) / args.length
    }
    return disp
}, 'unlim', 'var', Infinity)
const OPERATIONS = {
    '+': Add,
    '-': Subtract,
    '*': Multiply,
    '/': Divide,
    'negate': Negate,
    'sinh': Sinh,
    'cosh': Cosh,
    'mean': Mean,
    'var': Var
};
const ARITY = {
    '+': 2,
    '-': 2,
    '*': 2,
    '/': 2,
    'negate': 1,
    'sinh': 1,
    'cosh': 1,
    'mean': -1,
    'var': -1
}
const VARIABLES = {
    'x': 0,
    'y': 1,
    'z': 2
};
let expression = [];
let pos = 0;

function parseRecursive() {
    if (expression.length === 1) {
        if (expression[pos] in VARIABLES) {
            return new Variable(expression[pos]);
        } else if (!isNaN(expression[pos])) {
            return new Const(parseInt(expression[pos]));
        } else throw Error("wrong operation argument: " + expression[pos])
    } else {
        if (expression[pos] !== '(') {
            throw Error("lost open bracket")
        }
        let balance = 1;
        let operations = [];
        let i = 0;
        let cur = expression[pos + 1];
        pos += 2;
        while ((balance > 0 || ARITY[cur] > i)) {
            let oper;
            if (expression[pos] === ')') {
                balance--;
                if (balance === 0) {
                    break
                }
                pos++;
            }
            if (expression[pos] === '(') {
                balance++;
                oper = parseRecursive();
            } else if (expression[pos] in VARIABLES) {
                oper = new Variable(expression[pos]);
                pos++;
            } else if (!isNaN(expression[pos])) {
                oper = new Const(parseInt(expression[pos]));
                pos++;
            } else if (expression[pos] === ')') {
                balance--;
                if (balance === 0) {
                    break;
                }
            } else throw new Error("unknown symb: " + expression[pos]);
            operations[i] = oper;
            i++;
        }
        if (expression[pos] === ')') {
            balance--;
        } else throw new Error("lost right scobe")
        if (ARITY[cur] !== operations.length && ARITY[cur] !== -1) throw new Error("wrong count of arguments")
        return new OPERATIONS[cur](...operations)
    }
}

function parsePrefix(expr) {
    // console.log(expr)
    if (expr.length === 0) throw Error('empty input')
    let i = 0;
    let expressionBuilder = '';
    let lf = 0;
    let rt = 0;
    for (let j = 0; j < expr.length; j++) {
        if (expr[j] === ')' || expr[j] === '(') {
            expressionBuilder += ' '
            expressionBuilder += expr[j]
            expressionBuilder += ' '
        } else expressionBuilder += expr[j];
        if (expr[j] === ')') {
            rt++;
        }
        if (expr[j] === '(') {
            lf++;
        }
        if (lf < rt) {
            throw Error("left brackets less than right brackets")
        }
    }
    if (lf !== rt) {
        throw Error("left brackets more than right brackets")
    }
    expr = expressionBuilder;
    expression.length = 0;
    pos = 0;
    expr.trim().split(/\s+/).forEach(part => {
        expression[i] = part;
        i++;
    })
    let res = parseRecursive()
    if (pos !== expression.length - 1) throw Error("excessive info")
    return res
}

function parse(expr) {
    let res = [];
    expr.trim().split(/\s+/).forEach(part => {
        if (part in OPERATIONS) {
            let arguments = [];
            for (let j = 0; j < new OPERATIONS[part]().countOfArgs; j++) {
                arguments.push(res.pop());
            }
            if (arguments.length === 1) {
                res.push(new OPERATIONS[part](...arguments));
            } else {
                res.push(new OPERATIONS[part](arguments[1], arguments[0]))
            }
        } else if (part in VARIABLES) {
            res.push(new Variable(part));
        } else {
            res.push(new Const(parseInt(part)));
        }
    });
    return res.pop();
}