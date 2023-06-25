//
// Created by Andy Chow on 6/15/2022.
//

#ifndef KALEIDOSCOPE_MY_LANG_H
#define KALEIDOSCOPE_MY_LANG_H
#include <string>
#include <iostream>
#include <vector>
#include <memory>
#include <map>

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
/**
 * PrototypeAST - This class represents the "prototype" for a function, which 
 * captures its name, and its argument names (thus implicitly the number of 
 * arguments the function takes).
 */
class PrototypeAST;
// FunctionAST - This class represents a function definition itself.
class FunctionAST;

// getNextToken - Reads another token from the lexer and updates CurTok with its results.
static int getNextToken();
std::unique_ptr<ExprAST> LogError(const char *Str);
std::unique_ptr<PrototypeAST> LogErrorP(const char * Str);
static std::unique_ptr<ExprAST> ParseNumberExpr();
static std::unique_ptr<ExprAST> ParseParenExpr();
static std::unique_ptr<ExprAST> ParseIdentifierOrCallExpr();
static std::unique_ptr<ExprAST> ParsePrimary();

// GetTokPrecedence get the precedence of the pending binary operator token.
static int GetTokPrecedence();
static std::unique_ptr<ExprAST> ParseExpression();
static std::unique_ptr<ExprAST> ParseBinOpRHS(int ExprPrec, std::unique_ptr<ExprAST> LHS);
static std::unique_ptr<PrototypeAST> ParsePrototype();
static std::unique_ptr<FunctionAST> ParseDefinition();
static std::unique_ptr<PrototypeAST> ParseExtern();
static std::unique_ptr<FunctionAST> ParseTopLevelExpr();

static void InitializeModule();
static void HandleDefinition();
static void HandleExtern();
static void HandleTopLevelExpression();
static void MainLoop();


#endif //KALEIDOSCOPE_MY_LANG_H
