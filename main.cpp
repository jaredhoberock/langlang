#include "interpreter.hpp"
#include "parser.hpp"
#include "syntax.hpp"
#include <iostream>

void usage()
{
  std::cout << "usage: rb [script]" << std::endl;
}


bool interpret(interpreter& interp, const std::string& source)
{
  parser p{token_stream{source}};

  return p.parse().transform([&](program&& prog)
  {
    try
    {
      interp(prog);
    }
    catch(std::runtime_error& error)
    {
      std::cerr << "Runtime error: " << error.what() << std::endl;
      return false;
    }

    return true;
  })
  .transform_error([](std::string&& error)
  {
    auto message = std::format("Syntax error: {}", error);
    std::cerr << message << std::endl;
    return message;
  })
  .has_value();

//  bool result = true;
//
//  try
//  {
//    parser p{token_stream{source}};
//
//    auto prog = p.parse()
//      .transform([](program&& prog)
//      {
//        try
//        {
//          interp(prog);
//          return true;
//        }
//        catch(std::runtime_error& e)
//        {
//          std::cerr << "Runtime error: " << e.what() << std::endl;
//          return false;
//        }
//      })
//      .or_else([](std::string&& error)
//      {
//        std::cerr << "Syntax error: " << error.what() << std::endl;
//        return false;
//      })
//    ;
//
//    try
//    {
//      interp(prog);
//    }
//    catch(std::runtime_error& e)
//    {
//      std::cerr << "Runtime error: " << e.what() << std::endl;
//      result = false;
//    }
//  }
//  catch(std::runtime_error& e)
//  {
//    std::cerr << "Syntax error: " << e.what() << std::endl;
//    result = false;
//  }
//
//  return result;
}


bool interpret_from_file(const char* filename)
{
  std::ifstream file(filename);
  std::stringstream source;
  source << file.rdbuf();

  interpreter interp;
  return interpret(interp, source.str());
}


int interpret_from_prompt()
{
  interpreter interp;

  std::string line;

  std::cout << "> ";

  while(getline(std::cin, line))
  {
    interpret(interp, line);

    std::cout << "> ";
  }

  return 0;
}


int main(int argc, char** argv)
{
  if(argc > 2)
  {
    usage();
    return 0;
  }

  bool result = true;

  if(argc == 2)
  {
    result = interpret_from_file(argv[1]);
  }
  else
  {
    interpret_from_prompt();
  }

  return result ? 0 : -1;
}

