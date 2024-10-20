#include "interpreter.hpp"
#include "parsers.hpp"
#include "syntax.hpp"
#include <iostream>

void usage()
{
  std::cout << "usage: langlang [script]" << std::endl;
}


bool interpret(interpreter& interp, const std::string& source)
{
  // XXX wow this sucks
  std::vector<token> tokens;
  for(token_range rng{source}; auto t : rng)
  {
    tokens.push_back(t);
  }

  using namespace parsers;

  return parse_program(tokens).transform([&](success<program>&& result)
  {
    try
    {
      interp(result.value);
    }
    catch(std::runtime_error& error)
    {
      std::cerr << "Runtime error: " << error.what() << std::endl;
      return false;
    }

    return true;
  })
  .transform_error([](failure&& error)
  {
    token tok = error.remaining.front();

    auto message = std::format("Syntax error: {} at '{}': {}", tok.location(), tok.lexeme(), error.message);
    std::cerr << message << std::endl;
    return message;
  })
  .has_value();
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

