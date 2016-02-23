# CHa


CHa is a C compiler written entirely in Haskell ("C" + "Ha"skell = "CHa").
It started out as an *interpreter* for maximum uselessness, but then I realized
that some important things, e.g. `printf`, would be hard to do and even harder
to generalize. The ultimate goal is to be able to compile any and all preprocessed 
C code, but we'll see.


## Goals/Roadmap

##### Primary Goals

* [x] Define Haskell datatypes to represent the C grammar
* [ ] Parse source files into an AST
* [ ] Build the symbol table
* [ ] Typecheck
* [ ] Output LLVM assembly or x86_64 byte code (still undecided)

##### Stretch Goals
* [ ] Optimize


## Running
You can't yet. Submit a pull request, will ya?


## Testing
Haha, what's that?


### Disclaimer
For the love of God don't fucking compile anything important.
