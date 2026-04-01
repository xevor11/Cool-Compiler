# COOL Compiler (End-to-End)

This project implements a full compiler pipeline for the **COOL (Classroom Object-Oriented Language)**, covering all major compilation stages: lexical analysis, parsing, semantic analysis, and code generation.

---

## Overview

The compiler translates COOL programs into **MIPS assembly**, following a modular pipeline:
Source Code → Lexer → Parser → Semantic Analyzer → Code Generator → MIPS


Each stage is implemented from scratch using standard compiler tools and techniques.

---

## Features

### Lexer (Flex / Lex)
- Tokenizes COOL programs using regular expressions  
- Handles:
  - Keywords  
  - Identifiers  
  - Literals  
  - Edge cases (comments, strings)

---

### Parser (Bison / YACC)
- Implements an **LALR(1) grammar** for COOL  
- Resolves shift/reduce conflicts using:
  - Precedence rules  
  - Grammar restructuring  
- Builds an **Abstract Syntax Tree (AST)**  

---

### Semantic Analyzer (Scala)
- Performs type checking with **inheritance-aware environments**  
- Symbol resolution using **scoped environments**  
- Detects:
  - Circular inheritance  
  - Undefined classes  
  - Invalid method overrides  
- Annotates AST with:
  - Type information  
  - Binding information  

---

### Code Generator (Scala → MIPS)
- Generates **MIPS assembly** from typed AST  
- Implements:
  - Method dispatch (dynamic & static)  
  - Stack frame management (prologue/epilogue)  
  - Register handling and memory access  

---

## Key Design Concepts

### Environment Propagation
- Class environments are built hierarchically (**parent → child**)  
- Enables correct attribute and method resolution across inheritance  

---

### AST-Based Compilation
- All stages operate on structured **AST nodes**  
- Enables clean separation between:
  - Parsing  
  - Semantic analysis  
  - Code generation  

---

### Visitor Pattern
- Used extensively for:
  - Semantic analysis  
  - Code generation  
- Promotes modular and extensible design  

---

## Tech Stack

- **Languages:** Scala  
- **Tools:** Flex, Bison, YACC  
- **Target:** MIPS Assembly  

---

## Compilation Pipeline Summary
COOL Source Code
↓
Lexer
↓
Parser
↓
Semantic Analyzer
↓
Code Generator
↓
MIPS Assembly


---

## 📖 Notes

- This project emphasizes **from-scratch implementation** of compiler stages.  
- Designed for learning and understanding **core compiler principles**.  
- Modular structure allows easy extension and experimentation.

---
