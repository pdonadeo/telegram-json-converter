# telegram-json-converter

Convert the JSON dump from Telegram Desktop to decent HTML

## How to compile

This project is written in [OCaml](https://ocaml.org/) so you need a working
OCaml environment to compile. Luckily it's very simple.

1. Install OPAM following the [official guide](https://opam.ocaml.org/doc/Install.html).
   Every modern Linux distribution has an OPAM package.
2. create a separete switch with:
    ```bash
    $ opam switch --alias-of=4.06.1 4.06.1-TELEGRAM-JSON-CONVERTER
    ```
    the label `4.06.1-TELEGRAM-JSON-CONVERTER` is your choice. Remember the
    ```bach
    $ eval `opam config env`
    ```
    to activate the switch.
3. Install the required packages:
    ```bash
    $ opam install atdgen core core_extended jbuilder jingoo lens merlin \
        ocamlify ocp-indent ppx_deriving_yojson utop uunf uuseg uutf
    ```
4. Compile with:
    ```bash
    $ jbuilder build src/telegramjson2html.exe
    ```

## Developement status

The project is in a very early stage, not intended for daily use. It's largely incomplete and buggy. Be advised :wink:
