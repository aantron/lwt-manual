# Postprocessing and styling of the Lwt manual

The Lwt repo should be a sister directory of this repo `lwt-manual`.

To use, install the dependencies of Lwt. Jbuilder needs them to generate the ocamldoc output:

```
opam pin add -n lwt ../lwt
opam install --deps-only lwt
```

Jbuilder might also as you to install dependencies of `lwt_ssl`, `lwt_react`, etc. You can either install the packages as needed, or pin those Lwt subpackages and install that way:

```
opam pin add -n lwt_react ../lwt
opam install --deps-only lwt_react
# etc.
```

The postprocessor needs [Lambda Soup](https://github.com/aantron/lambda-soup) and [re](https://github.com/ocaml/ocaml-re):

```
opam install lambdasoup re
```

After that, running `make` should generate the postprocessed `Lwt.html`. Since I'm on a Mac, I also have the default target switch to Chrome, which I guess is usually called Google Chrome. Feel free to delete that line if it doesn't work for you, or to make any other edits to the `Makefile`.

The stylesheet is obviously `style.css`, and the postprocessor is `postprocess.ml`.

I've uploaded a branch `lwt.mli` to the Lwt repo. If you want to work with the new content, you'll want to pull that branch and switch to it before running `make`.
