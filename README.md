# JSyn
This crate is an attempt to convert the output of [syn](https://github.com/dtolnay/syn) (a rust parsing library) to the AST defined by [ressa](https://github.com/freemasen/ressa). That would allow a user to pass this AST to [resw](https://github.com/freemasen/resw) to generate javascript.

## But... why?
1. Wouldn't it be nice to have your rust `struct`s automatically converted into JavaScript `class`es? 
2. I'm sure there are other reasons...