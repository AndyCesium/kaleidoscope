//
// Created by Andy Chow on 6/15/2022.
//

#ifndef KALEIDOSCOPE_MY_LANG_H
#define KALEIDOSCOPE_MY_LANG_H
#include <iostream>
#include <memory>

// gettok - Return the next token from standard input.
static int gettok();

// ExprAST - Base class for all expression nodes
class ExprAST;
// NumberExprAST - Expression class for numeric literals
class NumberExprAST;
// VariableExprAST - Expression class for referencing a variable
class VariableExprAST;
// BinaryExprAST - Expression class for a binary operator.
class BinaryExprAST;
// CallExprAST - Expression class for function calls.
class CallExprAST;
// IfExprAST - Expression class for if/then/else.
class IfExprAST;
// ForExprAST - Expression class for for/in.
class ForExprAST;
/**
 * PrototypeAST - This class represents the "prototype" for a function, which 
 * captures its name, and its argument names (thus implicitly the number of 
 * arguments the function takes).
 */
class PrototypeAST;
// FunctionAST - This class represents a function definition itself.
class FunctionAST;

/**
 * getNextToken - Reads another token from the lexer and updates CurTok with 
 * its results.
 */
static int getNextToken();
// GetTokPrecedence - get the precedence of the pending binary operator token.
static int GetTokPrecedence();
// LogError - Helper function for error handling.
std::unique_ptr<ExprAST> LogError(const char *Str);
std::unique_ptr<PrototypeAST> LogErrorP(const char * Str);
// numberexpr ::= number
static std::unique_ptr<ExprAST> ParseNumberExpr();
// parenexpr ::= '(' expression ')'
static std::unique_ptr<ExprAST> ParseParenExpr();
/**
 * identifierexpr
 *     ::= identifier
 *     ::= identifier '(' expression* ')'
 */
static std::unique_ptr<ExprAST> ParseIdentifierExpr();
// ifexpr ::= 'if' expression 'then' expression 'else' expression
static std::unique_ptr<ExprAST> ParseIfExpr();
// forexpr ::= 'for' identifier '=' expr ',' expr (',' expr)? 'in' expression
static std::unique_ptr<ExprAST> ParseForExpr();
/**
 * primary
 *     ::= identifierexpr
 *     ::= numberexpr
 *     ::= parenexpr
 *     ::= ifexpr
 *     ::= forexpr
 */
static std::unique_ptr<ExprAST> ParsePrimary();

// binoprhs ::= ('+' primary)*
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);
// expression ::= primary binoprhs
static std::unique_ptr<ExprAST> ParseExpression();
// prototype ::= id '(' id* ')'
static std::unique_ptr<PrototypeAST> ParsePrototype();
// definition ::= 'def' prototype expression
static std::unique_ptr<FunctionAST> ParseDefinition();
// external ::= 'extern' prototype
static std::unique_ptr<PrototypeAST> ParseExtern();
// toplevelexpr ::= expression
static std::unique_ptr<FunctionAST> ParseTopLevelExpr();

static void InitializeModuleAndPassManager();
static void HandleDefinition();
static void HandleExtern();
static void HandleTopLevelExpression();
static void MainLoop();


#endif //KALEIDOSCOPE_MY_LANG_H
