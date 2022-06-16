//
// Created by Andy Chow on 6/15/2022.
//

#include "my_lang.h"
#include <string>
#include <iostream>
#include <vector>
#include <memory>

using namespace std;

enum Token {
    tok_eof = -1,

    // commands
    tok_def = -2,
    tok_extern = -3,

    // primary
    tok_identifier = -4,
    tok_number = -5,
};

static string IdentifierStr;        // Filled in if tok_identifier
static double NumVal;               // Filled in if tok_number

static int gettok() {
    static int LastChar = ' ';

    while (isspace(LastChar)) {
        LastChar = getchar();
    }

    // Get identifier
    if (isalpha(LastChar)) {
        IdentifierStr = LastChar;
        while (isalnum((LastChar = getchar()))) {
            IdentifierStr += LastChar;
        }

        if (IdentifierStr == "def") {
            return tok_def;
        }
        if (IdentifierStr == "extern") {
            return tok_extern;
        }
        return tok_identifier;
    }

    // Get number
    if (isdigit(LastChar) || LastChar == '.') {
        string NumStr;
        do {
            NumStr += LastChar;
            LastChar = getchar();
        } while (isdigit(LastChar) || LastChar == '.');
        NumVal = strtod(NumStr.c_str(), 0);
        return tok_number;
    }

    // Get comment
    if (LastChar == '#') {
        do {
            LastChar = getchar();
        } while (LastChar != EOF && LastChar != '\n' && LastChar != '\r');

        if (LastChar != EOF) {
            return gettok();
        }
    }

    if (LastChar == EOF) {
        return tok_eof;
    }

    int ThisChar = LastChar;
    LastChar = getchar();
    return ThisChar;
}

/// ExprAST - Base class for all expression nodes.
class ExprAST {
public:
    virtual ~ExprAST() {}
};

/// NumberExprAST - Expression class for numeric literals
class NumberExprAST : public ExprAST {
    double Val;

public:
    NumberExprAST(double V) : Val(V) {}
};

/// VariableExprAST - Expression class for referencing a variable
class VariableExprAST : public ExprAST {
    string Name;

public:
    VariableExprAST(const string &Name) : Name(Name) {}
};

/// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST : public ExprAST {
    char Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char op, unique_ptr<ExprAST> LHS, unique_ptr<ExprAST> RHS)
            : Op(op), LHS(move(LHS)), RHS(move(RHS)) {}
};

/// CallExprAST - Expression class for function calls.
class CallExprAST : public ExprAST {
    string Callee;
    vector<unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const string &Callee,
                vector<unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(move(Args)) {}
};

/// PrototypeAST - This class represents the "prototype" for a function,
/// which captures its name, and its argument names (thus implicitly the number
/// of arguments the function takes).
class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args)
            : Name(name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
};

/// FunctionAST - This class represents a function definition itself.
class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

/// CurTok is the current token the parser is looking at.
/// getNextToken reads another token from the lexer and updates CurTok with its results.
static int CurTok;
static int getNextTokken() {
    return CurTok = gettok();
}

unique_ptr<ExprAST> LogError(const char *Str) {
    fprintf(stderr, "LogError: %s\n", Str);
    return nullptr;
}

unique_ptr<PrototypeAST> LogErrorP(const char * Str) {
    LogError(Str);
    return nullptr;
}

static unique_ptr<ExprAST> ParseNumberExpr() {
    auto Result = make_unique<NumberExprAST>(NumVal);
    getNextTokken();
    return move(Result);
}

static unique_ptr<ExprAST> ParseParenExpr() {
    getNextTokken();            // Eat '('
    auto V = ParseExpression();
    if (!V) {
        return nullptr;
    }

    if (CurTok == ')') {
        getNextTokken();        // Eat ')'
        return V
    } else {
        return LogError("expect ')'");
    }
}

static unique_ptr<ExprAST> ParseIdentifierOrCallExpr() {
    string IdName = IdentifierStr;

    getNextTokken();        // Eat the identifier

    // Check if this is a function call
    if (CurTok == '(') {
        getNextTokken();    // Eat '('
        vector<unique_ptr<ExprAST>> Args;

        while (true) {
            auto Arg = ParseExpression();
            if (Arg) {
                Args.push_back(Arg);
            } else {
                return nullptr;
            }

            if (CurTok == ')') {
                getNextTokken();    // Eat ')'
                break;
            }

            if (CurTok == ',') {
                getNextTokken();    // Eat ','
                continue;
            } else {
                LogError("Expected ')' or ',' in argument list");
            }
        }

        return make_unique<CallExprAST>(IdName, move(Args));
    }
    // this is an identifier...
    else {
        return make_unique<VariableExprAST>(IdName);
    }
}

static std::unique_ptr<ExprAST> ParsePrimary() {
    switch (CurTok) {
        case tok_identifier:
            return ParseIdentifierOrCallExpr();
        case tok_number:
            return ParseNumberExpr();
        case '(':
            return ParseParenExpr();
        default:
            return LogError("unknown token when expecting an expression");
    }
}

/*
int main() {
    while (true) {
        int tok = gettok();
        cout << "get token: " << tok << endl;
    }
}
 */
