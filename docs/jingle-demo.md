# jingle-demo

`jingle-demo` -- The jingle demo app

## Usage

``` shell
jingle-demo [global-options] [<command>] [command-options] [arguments ...]
```

## Options

`jingle-demo` accepts the following options:

``` shell
      --address <VALUE>  hostname of the API server [default: 127.0.0.1] [env: $ENDPOINT]
      --help             display usage information and exit
      --port <INT>       port on which the API server is listening [default: 5000] [env: $PORT]
      --scheme <CHOICE>  scheme to use when connecting to the API server [default: http]
                         [choices: http, https]
      --version          display version and exit

```

## Sub Commands

`jingle-demo` provides the following sub commands:

``` shell
  serve            start the HTTP server
  list             list products
  get              get products by id
  ping             ping the remote API endpoint
  delete           delete products by id
  create           create new products
  print-doc        print the documentation in Markdown format
  zsh-completions  generate the Zsh completion script

```

## Authors

* Marin Atanasov Nikolov <dnaeon@gmail.com>

## License

BSD 2-Clause

# jingle-demo serve

`jingle-demo serve` -- start the HTTP server

## Usage

``` shell
jingle-demo [global-options] serve [options] [arguments ...]
```

## Options

`jingle-demo serve` accepts the following options:

``` shell
      --address <VALUE>  address to bind to [default: 127.0.0.1] [env: $ADDRESS]
      --help             display usage information and exit
      --port <INT>       port to listen on [default: 5000] [env: $PORT]
      --silent <VALUE>   run in silent mode, if set to true [default: FALSE] [env: $SILENT]
      --version          display version and exit

```

# jingle-demo list

`jingle-demo list` -- list products

## Usage

``` shell
jingle-demo [global-options] list [options] [arguments ...]
```

## Options

`jingle-demo list` accepts the following options:

``` shell
      --from <INT>  fetch products starting from this offset [default: 0]
      --help        display usage information and exit
      --to <INT>    fetch products up to this offset [default: 2]
      --version     display version and exit

```

# jingle-demo get

`jingle-demo get` -- get products by id

## Usage

``` shell
jingle-demo get [id] ...
```

## Options

`jingle-demo get` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# jingle-demo ping

`jingle-demo ping` -- ping the remote API endpoint

## Usage

``` shell
jingle-demo [global-options] ping [options] [arguments ...]
```

## Options

`jingle-demo ping` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# jingle-demo delete

`jingle-demo delete` -- delete products by id

## Usage

``` shell
jingle-demo delete [id] ...
```

## Options

`jingle-demo delete` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# jingle-demo create

`jingle-demo create` -- create new products

## Usage

``` shell
jingle-demo create [name] ...
```

## Options

`jingle-demo create` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# jingle-demo print-doc

`jingle-demo print-doc` -- print the documentation in Markdown format

## Usage

``` shell
jingle-demo print-doc 
```

## Options

`jingle-demo print-doc` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

# jingle-demo zsh-completions

`jingle-demo zsh-completions` -- generate the Zsh completion script

## Usage

``` shell
jingle-demo zsh-completions 
```

## Options

`jingle-demo zsh-completions` accepts the following options:

``` shell
      --help     display usage information and exit
      --version  display version and exit

```

