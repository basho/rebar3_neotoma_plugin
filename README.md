# rebar3_neotoma_compiler
Rebar3 neotoma (Parser Expression Grammar) compiler.

Refer to the [GitHub Repository](https://github.com/basho/neotoma) for details on what `neotoma` does.

## How To Use It

The plugin is configured with statements in your `rebar.config` file.

### To Include the Plugin

```erlang
{plugins, [
    {rebar3_neotoma_plugin,
        {git, "https://github.com/basho/rebar3_neotoma_plugin.git",
        {branch, "master"} }}
]}.
```

### To Compile `.peg` Files Automatically

```erlang
{provider_hooks, [
    {pre,   [{compile,  neotoma}]}
]}.
```
> Note that unlike the original from which this is forked, this provider resides in the default namespace.

## Command Line

### To Compile `.peg` Files Manually

```shell
$ rebar3 neotoma
```

## Notes

This revision uses [Basho's](http://www.basho.com) fork of _neotoma_, which may differ from the upstream original.

### Development

To run `xref` or `dialyzer` on the plugin, use the `check` profile:

```shell
$ rebar3 as check xref
$ rebar3 as check dialyzer
```

## License

This work is heavily revised from earlier versions by Oleg Tsarev and Tristan Sloughter.

All revisions are subject to this [license](LICENSE).
