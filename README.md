# Extended Refal interpreter in Haskell
This is an implementation of an Extended Refal interpreter in Haskell. Refal is a functional programming language, designed around pattern matching.

# Running
[![Open in GitHub Codespaces](https://github.com/codespaces/badge.svg)](https://codespaces.new/uf5/refal?quickstart=1)
## Using Nix
```shell
nix run
```
## Using Cabal
```shell
cabal run
```

# Examples
See `/examples` folder

# Standard Functions
- `Add sm sn`
    - Returns `sm` + `sn`.
- `Sub sm sn`
    - Returns `sm` - `sn`.
- `Mul sm sn`
    - Returns `sm` * `sn`.
- `Div sm sn`
    - Returns integer result of `sm` / `sn`. Throws an error if `sn` is zero.
- `Mod sm sn`
    - Returns the remainder of `sm` divided by `sn`. Throws an error if `sn` is zero.
- `Compare sm sn`
    - Returns
        - "-" if `sm` < `sn`.
        - "=" if `sm` = `sn`.
        - "+" if `sm` > `sn`.
- `Symb sm`
    - Converts an `sm` to a character.
- `Numb sm`
    - Converts a `sm` to an integer.
- `Upper sm`
    - Converts a `sm` to uppercase.
- `Lower sm`
    - Converts a `sm` to lowercase.
- `Lenw ea`
    - Returns the length of `ea`.
- `Type ta`
    - Returns
        - "D" if `ta` is an integer
        - "L" if `ta` is a char
        - "I" if `ta` is an identifier
        - "S" if `ta` is a `( .. )`
- `Mu (e.fname) e.args`
    - Applies function with name `e.fname` to `e.args`.
    - Returns the result of function application.
- `Print ea`
    - Prints `ea`.
    - Returns `ea`.
- `Prout ea`
    - Prints `ea`.
    - Returns nothing.
- `Br (e.name) e.value`
    - Buries a value e.value with name `e.name`.
- `Dg e.name`
    - Digs up a buried value with name `e.name`.
- `Cp e.name`
    - Same as `Dg` but without removing the value from the stack.
- `Rp (e.name) e.value`
    - Replaces value with name `e.name` with `e.value`.

# Differences from the original Refal
- No `#ENTRY` pragma, entry point is defined by a function named `Main`.
- No macrodigits (big integers consisting of multiple base $2^{32}$ integers), instead Haskell's builtin big integer `Integer` type is used.
