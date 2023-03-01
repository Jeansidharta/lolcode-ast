# ⚠️ WIP ⚠️

This project is currently a work in progress. It'll somewhat work, but it's not
properly tested or documented. Clone at your own risk

# LOLCODE AST

This is a lolcode AST generator written in rust. It's intended purpose is to
assist in other projects of mine involving LOLCODE.

## What is LOLCODE?

LOLCODE ([Wikipedia page](https://en.wikipedia.org/wiki/LOLCODE)) is an exoteric
language created in 2007 by [Justin Meza](https://github.com/justinmeza)
inspired by [lolspeak](https://en.wikipedia.org/wiki/Lolcat), an internet meme
of the time. Justin wrote a
[specification](https://github.com/justinmeza/lolcode-spec) for the language and
an [interpreter](https://github.com/justinmeza/lci) written in C.

## How to build

- Instal the rust toolchain (link
  [here](https://www.rust-lang.org/tools/install)) if you don't have it
  installed already.

- Clone this project

- `cargo build --release`

- Your binary will be located at `./target/release/lolcode_ast`

## Contributing

This project is mainly for personal use, but I'll gladly accept pull requests,
and read issues. For more information, read the [CONTRIBUTING](CONTRIBUTING.md)
file.

## License

This code is under the GPL 3 license
