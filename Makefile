CXX := clang++-19

langlang: main.cpp interpreter.hpp parser.hpp syntax.hpp token_stream.hpp Makefile
	$(CXX) $(SANITIZE) -std=c++23 -Wall -Wextra -Werror main.cpp -lstdc++ -lfmt -g -o langlang

clean:
	rm -rf *.o langlang
