json-test
==================

A specification abiding implementation of [JSON](https://www.json.org/) [RFC-7159](https://tools.ietf.org/html/rfc7159)
using [alex](https://www.haskell.org/alex/) and [happy](https://www.haskell.org/happy/).

### Usage

Given an example `JSON` file.
```json
{ "foo" : [{"bar": [1,2,true,false, {}]}], "baz": { "woz" : [] } }
```

The following code will parse the above into `aeson`'s `Value` type (via `alex` and `happy`).
```haskell
module Main (main) where

import JSON.Parser (parse)

parseWithJSONTest :: IO ()
parseWithJSONTest =
  print =<< parse <$> readFile fileName
```

to yield

```haskell
Right (Object (fromList [
  ("foo", Array [Object (fromList [
    ("bar",Array [Number 1.0,Number 2.0,Bool True,Bool False,Object (fromList [])])])])
   ,("baz",Object (fromList [("woz",Array [])
  ]))
]))
```

### Build
```bash
nix-build
```

### Develop
```bash
nix-shell
```

### Benchmark
```bash
nix-build && ./result/bin/example -o index.html && open index.html
```
