# rebar3_neotoma_compiler
Rebar3 neotoma (Parser Expression Grammar) compiler.

Refer to the [GitHub Repository](https://github.com/basho/neotoma) for details on what `neotoma` does.

## How To Use It

Include the following in your `rebar.config` file:

```erlang
{plugins, [
    {rebar3_neotoma_plugin,
        {git, "https://github.com/basho/rebar3_neotoma_plugin.git",
        {branch, "master"} }}
]}.
```

### Notes

This fork uses [Basho's](http://www.basho.com) fork of _neotoma_, which may differ from the upstream original.

To run `xref` or `dialyzer` on the plugin, use the `check` profile.

### License

Refer to the license [here](LICENSE).
