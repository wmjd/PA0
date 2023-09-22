%.run: %.o
	clang -o $@ main.c $<

%.o: %.s
	nasm -f macho64 -o $@ $<
	# Use macho64 for OSX or elf64 for Linux

%.s: %.int
	ocaml compile.ml $< > $@

.PHONY: clean

clean:
	-rm *.o
	-rm *.s
	-rm *.run
