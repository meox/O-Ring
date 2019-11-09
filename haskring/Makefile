all:
	stack build && stack install
	mv ~/.local/bin/haskring ring
clean:
	stack clean
	rm -f ring

.PHOENY: all
