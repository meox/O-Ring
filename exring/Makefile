all:
	ERL_COMPILER_OPTIONS="native" MIX_ENV=prod mix escript.build
	mv exring ring 
clean:
	rm -f ring exring

.PHOENY: all
