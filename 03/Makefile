default:
	@echo available targets:
	@echo - null_java
	@echo - null_ruby
	@echo - mergesort
	@echo - types.js

null_java:
	javac NullPointer.java
	java NullPointer

null_ruby:
	ruby null_pointer.rb

types.js: types.ts
	tsc types.ts

mergesort:
	sml mergesort.sml

clean:
	rm -rf *class
