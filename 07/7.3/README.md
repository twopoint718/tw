# Haskell-to-Elm

Generate Elm source code type definitions based on haskell types via Generics.
Based upon work by:

- [Types All the Way Down](https://www.youtube.com/watch?v=sh4H8yzXnvw)
  by Kris Jenkins
- [Cooking Classes with Datatype Generic Programming](http://www.stephendiehl.com/posts/generics.html)
  by Stephen Diehl

## Running

Prerequisites:

- [Elm](https://guide.elm-lang.org/install.html) is installed
- [Haskell Stack](https://docs.haskellstack.org/en/stable/README/#how-to-install) is installed
- [PostgreSQL]() is installed and running with a DB called `generic-ride`.
    ```
	> createdb generic-ride
	```

Then, the Makefile should take care of everything:

```
> make
> make run
```
