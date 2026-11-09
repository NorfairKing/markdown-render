# main: Fix the cache key

- **Commit**: `1a2b3c4`
- **Message**: Fix the cache key
  \* It was \[wrong](no)
  \# for \`nix` 2.18
- **Author**: Jane Doe <jane@example.com\>

## Runs

- [success] [build packages.x86_64-linux.default](https://ci.example.com/run/1)
- [failure] [test checks.x86_64-linux.e2e_test](https://ci.example.com/run/2) (attempt 2)
  - Dependencies: `packages.x86_64-linux.default`

## Reproduce

```bash
nix build .#checks.x86_64-linux.e2e_test
```

> Logs are at [the run page](https://ci.example.com/run/2).
