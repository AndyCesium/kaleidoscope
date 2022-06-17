//
// Created by Andy Chow on 6/15/2022.
//

#include "my_lang.h"
#include <string>
#include <iostream>
#include <vector>
#include <memory>
#include <map>

using namespace std;

//===----------------------------------------------------------------------===//
// Lexer
//===----------------------------------------------------------------------===//

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

//===----------------------------------------------------------------------===//
// Abstract Syntax Tree
//===----------------------------------------------------------------------===//

class ExprAST {
public:
    virtual ~ExprAST() {}
};

class NumberExprAST : public ExprAST {
    double Val;

public:
    NumberExprAST(double V) : Val(V) {}
};

class VariableExprAST : public ExprAST {
    string Name;

public:
    VariableExprAST(const string &Name) : Name(Name) {}
};

class BinaryExprAST : public ExprAST {
    char Op;
    unique_ptr<ExprAST> LHS, RHS;

public:
    BinaryExprAST(char op, unique_ptr<ExprAST> LHS, unique_ptr<ExprAST> RHS)
            : Op(op), LHS(move(LHS)), RHS(move(RHS)) {}
};

class CallExprAST : public ExprAST {
    string Callee;
    vector<unique_ptr<ExprAST>> Args;

public:
    CallExprAST(const string &Callee,
                vector<unique_ptr<ExprAST>> Args)
            : Callee(Callee), Args(move(Args)) {}
};

class PrototypeAST {
    std::string Name;
    std::vector<std::string> Args;

public:
    PrototypeAST(const std::string &name, std::vector<std::string> Args)
            : Name(name), Args(std::move(Args)) {}

    const std::string &getName() const { return Name; }
};

class FunctionAST {
    std::unique_ptr<PrototypeAST> Proto;
    std::unique_ptr<ExprAST> Body;

public:
    FunctionAST(std::unique_ptr<PrototypeAST> Proto,
                std::unique_ptr<ExprAST> Body)
            : Proto(std::move(Proto)), Body(std::move(Body)) {}
};

//===----------------------------------------------------------------------===//
// Parser
//===----------------------------------------------------------------------===//

/// CurTok is the current token the parser is looking at.
static int CurTok;
static int getNextToken() {
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
    getNextToken();
    return move(Result);
}

static unique_ptr<ExprAST> ParseParenExpr() {
    getNextToken();            // Eat '('
    auto V = ParseExpression();
    if (!V) {
        return nullptr;
    }

    if (CurTok == ')') {
        getNextToken();        // Eat ')'
        return V;
    } else {
        return LogError("expect ')'");
    }
}

static unique_ptr<ExprAST> ParseIdentifierOrCallExpr() {
    string IdName = IdentifierStr;

    getNextToken();        // Eat the identifier

    // Check if this is a function call
    if (CurTok == '(') {
        getNextToken();    // Eat '('
        vector<unique_ptr<ExprAST>> Args;

        while (true) {
            auto Arg = ParseExpression();
            if (Arg) {
                Args.push_back(move(Arg));
            } else {
                return nullptr;
            }

            if (CurTok == ')') {
                getNextToken();    // Eat ')'
                break;
            }

            if (CurTok == ',') {
                getNextToken();    // Eat ','
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

static unique_ptr<ExprAST> ParsePrimary() {
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

/// BinopPrecedence holds the precedence for each binary operator that is defined.
static map<char, int> BinopPrecedence;

static int GetTokPrecedence() {
    if (!isascii(CurTok))
        return -1;

    // Make sure it's a declared binop.
    int TokPrec = BinopPrecedence[CurTok];
    if (TokPrec <= 0) return -1;
    return TokPrec;
}

static unique_ptr<ExprAST> ParseExpression() {
    auto LHS = ParsePrimary();
    if (LHS) {
        return ParseBinOpRHS(0, move(LHS));
    }
    return nullptr;
}

static unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, unique_ptr<ExprAST> LHS) {
    // If this is a binop, find its precedence.
    while (true) {
        int TokPrec = GetTokPrecedence();

        // If this is a binop that binds at least as tightly as the current binop,
        // consume it, otherwise we are done.
        if (TokPrec < ExprPrec) {
            return LHS;
        }

        // We know this is a binop.
        int BinOp = CurTok;
        getNextToken();  // eat binop

        // Parse the primary expression after the binary operator.
        auto RHS = ParsePrimary();
        if (!RHS) {
            return nullptr;
        }

        // If BinOp binds less tightly with RHS than the operator after RHS, let
        // the pending operator take RHS as its LHS.
        int NextPrec = GetTokPrecedence();
        if (TokPrec < NextPrec) {
            RHS = ParseBinOpRHS(TokPrec+1, std::move(RHS));
            if (!RHS){
                return nullptr;
            }
        }

        // Merge LHS/RHS.
        LHS = make_unique<BinaryExprAST>(BinOp, move(LHS), move(RHS));
    }
}

static unique_ptr<PrototypeAST> ParsePrototype() {
    if (CurTok != tok_identifier) {
        return LogErrorP("Expected '(' in prototype");
    }

    string FnName = IdentifierStr;
    getNextToken();  // eat identifier

    vector<string> ArgNames;
    while (getNextToken() == tok_identifier) {
        ArgNames.push_back(IdentifierStr);
    }
    if (CurTok != ')') {
        return LogErrorP("Expected ')' in prototype");
    }

    getNextToken();

    return make_unique<PrototypeAST>(FnName, move(ArgNames));
}

static unique_ptr<FunctionAST> ParseDefinition() {
    getNextToken();  // eat def
    auto Proto = ParsePrototype();
    if (!Proto) {
        return nullptr;
    }

    auto E = ParseExpression();
    if (E) {
        return make_unique<FunctionAST>(move(Proto),move(E));
    } else {
        return nullptr;
    }
}

static unique_ptr<PrototypeAST> ParseExtern() {
    getNextToken();  // get extern
    return ParsePrototype();
}

static unique_ptr<FunctionAST> ParseTopLevelExpr() {
    auto E = ParseExpression();
    if (E) {
        // Make an  anonymous proto.
        auto Proto = make_unique<PrototypeAST>("", vector<string>());
        return make_unique<FunctionAST>(move(Proto), move(E));
    }
    return nullptr;
}

//===----------------------------------------------------------------------===//
// Top-Level parsing
//===----------------------------------------------------------------------===//

static void HandleDefinition() {
    if (ParseDefinition()) {
        fprintf(stderr, "Parsed a function definition.\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleExtern() {
    if (ParseExtern()) {
        fprintf(stderr, "Parsed an extern\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void HandleTopLevelExpression() {
    // Evaluate a top-level expression into an anonymous function.
    if (ParseTopLevelExpr()) {
        fprintf(stderr, "Parsed a top-level expr\n");
    } else {
        // Skip token for error recovery.
        getNextToken();
    }
}

static void MainLoop() {
    while (true) {
        switch (CurTok) {
            case tok_eof:
                return;
            case ';': // ignore top-level semicolons.
                fprintf(stderr, "ready>");
                getNextToken();
                break;
            case tok_def:
                HandleDefinition();
                break;
            case tok_extern:
                HandleExtern();
                break;
            default:
                HandleTopLevelExpression();
                break;
        }
    }
}

//===----------------------------------------------------------------------===//
// Main driver code.
//===----------------------------------------------------------------------===//

int main() {
    // Install standard binary operators.
    BinopPrecedence['<'] = 10;  // lowest.
    BinopPrecedence['+'] = 20;
    BinopPrecedence['-'] = 20;
    BinopPrecedence['*'] = 40;  // highest.

    // Prime the first token.
    fprintf(stderr, "ready>");
    getNextToken();

    // Run the main "interpreter loop".
    MainLoop();

    return 0;
}
