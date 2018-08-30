# Sometimes build artifacts don't get cleaned up correctly
clean:
	rm -f a*.out; rm -rf build*
.PHONY: clean
