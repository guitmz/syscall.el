# Contributing

Thanks for helping out. Keep changes small, keep Emacs 29.1 support, and please don't add required packages.

## Running things

```sh
make dataset   # refresh the bundled data from the API
make compile   # byte compile, warnings fail the build
make test      # ERT suite, no network needed
make lint      # checkdoc and package-lint
make live      # two real updates against the API into a throwaway cache
```

Tests never touch the network. They swap `syscall--http-get` for a fake server, so mock there when you need new API behaviour.

## Style

- Lexical binding everywhere
- Public names use `syscall-`, internal helpers use `syscall--`
- Public commands and options get docstrings written for users
- Anything that hits the network goes through the update job, so the rate limit, retries and staging step keep working
- Lookups must work from the cache alone and never prompt for a download from ElDoc or other background code

Before opening a PR, run `make compile test` and make sure edited files have no trailing whitespace and end with a newline.
