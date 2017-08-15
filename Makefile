FILE := Lwt.html

.PHONY : do-it
do-it :
	ocamlfind c -linkpkg -package lambdasoup,re postprocess.ml
	make -C ../lwt doc-api-html
	./a.out < ../lwt/doc/api/html/Lwt.html > $(FILE)
	open -a Chrome

.PHONY : clean
clean :
	rm -f a.out *.cm* $(FILE)
