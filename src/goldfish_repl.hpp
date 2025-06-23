//
// Copyright (C) 2024 The Goldfish Scheme Authors
//
// Licensed under the Apache License, Version 2.0 (the "License");
// you may not use this file except in compliance with the License.
// You may obtain a copy of the License at
//
// http://www.apache.org/licenses/LICENSE-2.0
//
// Unless required by applicable law or agreed to in writing, software
// distributed under the License is distributed on an "AS IS" BASIS,
// WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
// See the License for the specific language governing permissions and
// limitations under the License.
//

#pragma once

// 只有定义了GOLDFISH_ENABLE_REPL宏时才编译REPL功能
#ifdef GOLDFISH_ENABLE_REPL

#include <iostream>
#include <s7.h>
#include <sstream>
#include <string>

// Include necessary definitions from goldfish.hpp
#ifndef GOLDFISH_VERSION
#define GOLDFISH_VERSION "17.11.17"
#endif

// macOS specific headers for readline support
#if defined(TB_CONFIG_OS_MACOSX) || defined(TB_CONFIG_OS_LINUX) || defined(__linux__)
#include <readline/history.h>
#include <readline/readline.h>
#define GOLDFISH_HAS_READLINE 1
#else
#define GOLDFISH_HAS_READLINE 0
#endif

namespace goldfish {

using std::cerr;
using std::cin;
using std::cout;
using std::endl;
using std::string;

// Interactive REPL implementation
inline int
interactive_repl (s7_scheme* sc, const string& mode) {
  cout << "Goldfish Scheme " << GOLDFISH_VERSION << " by LiiiLabs" << endl;
  cout << "Based on S7 Scheme " << S7_VERSION << " (" << S7_DATE << ")" << endl;
  cout << "Mode: " << mode << endl;
  cout << "Type (exit) or press Ctrl+D to quit." << endl;
#if GOLDFISH_HAS_READLINE
  cout << "Readline support enabled - use arrow keys for history navigation." << endl;
#endif
  cout << endl;

  string input_line;
  string accumulated_input;
  int    line_num     = 1;
  bool   in_expression= false;
  int    paren_count  = 0;

#if GOLDFISH_HAS_READLINE
  // Use readline for better input experience on Unix-like systems
  char*  line;
  string prompt;

  while (true) {
    // Create prompt
    if (in_expression) {
      prompt= "  ... ";
    }
    else {
      prompt= "gf> ";
    }

    // Read line with readline
    line= readline (prompt.c_str ());

    // Check for EOF (Ctrl+D)
    if (!line) {
      cout << endl << "Goodbye!" << endl;
      break;
    }

    input_line= string (line);
    free (line);

    // Add non-empty lines to history
    if (!input_line.empty ()) {
      add_history (input_line.c_str ());
    }

    // Check for exit command
    if (input_line == "(exit)" || input_line == "exit") {
      cout << "Goodbye!" << endl;
      break;
    }

    // Skip empty lines
    if (input_line.empty () && !in_expression) {
      continue;
    }

    // Count parentheses to handle multi-line expressions
    for (char c : input_line) {
      if (c == '(') paren_count++;
      else if (c == ')') paren_count--;
    }

    // Accumulate input
    if (accumulated_input.empty ()) {
      accumulated_input= input_line;
    }
    else {
      accumulated_input+= "\n" + input_line;
    }

    // Check if we have a complete expression
    if (paren_count <= 0 && !accumulated_input.empty ()) {
      in_expression= false;

      // Evaluate the accumulated input
      s7_pointer result= s7_eval_c_string (sc, accumulated_input.c_str ());
      if (result) {
        char* result_str= s7_object_to_c_string (sc, result);
        if (result_str) {
          cout << result_str << endl;
          free (result_str);
        }
      }
      else {
        cerr << "Error: Failed to evaluate expression" << endl;
      }

      // Reset for next expression
      accumulated_input.clear ();
      paren_count= 0;
    }
    else if (paren_count > 0) {
      in_expression= true;
    }

    line_num++;
  }

#else
  // Fallback simple REPL for platforms without readline
  cout << "Basic input mode - readline not available." << endl;
  
  while (true) {
    // Display prompt
    if (in_expression) {
      cout << "  ... ";
    }
    else {
      cout << "gf> ";
    }

    // Read line
    if (!getline (cin, input_line)) {
      cout << endl << "Goodbye!" << endl;
      break;
    }

    // Check for exit command
    if (input_line == "(exit)" || input_line == "exit") {
      cout << "Goodbye!" << endl;
      break;
    }

    // Skip empty lines
    if (input_line.empty () && !in_expression) {
      continue;
    }

    // Count parentheses to handle multi-line expressions
    for (char c : input_line) {
      if (c == '(') paren_count++;
      else if (c == ')') paren_count--;
    }

    // Accumulate input
    if (accumulated_input.empty ()) {
      accumulated_input= input_line;
    }
    else {
      accumulated_input+= "\n" + input_line;
    }

    // Check if we have a complete expression
    if (paren_count <= 0 && !accumulated_input.empty ()) {
      in_expression= false;

      // Evaluate the accumulated input
      s7_pointer result= s7_eval_c_string (sc, accumulated_input.c_str ());
      if (result) {
        char* result_str= s7_object_to_c_string (sc, result);
        if (result_str) {
          cout << result_str << endl;
          free (result_str);
        }
      }
      else {
        cerr << "Error: Failed to evaluate expression" << endl;
      }

      // Reset for next expression
      accumulated_input.clear ();
      paren_count= 0;
    }
    else if (paren_count > 0) {
      in_expression= true;
    }

    line_num++;
  }
#endif

  return 0;
}
} // namespace goldfish

#endif // GOLDFISH_ENABLE_REPL
