all:
	docker run -v `pwd`:/src/main ponylang/ponyc:0.33.0
	mv main ring

clean:
	rm -f ring

.PHONY: all

