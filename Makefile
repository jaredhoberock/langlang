CXX := clang++-19

langlang: main.cpp interpreter.hpp parsers.hpp source_location.hpp syntax.hpp token_range.hpp Makefile
	$(CXX) $(SANITIZE) -std=c++23 -Wall -Wextra -Werror main.cpp -lstdc++ -lfmt -g -o langlang

clean:
	rm -rf *.o langlang
