# Haskell in browser

This is a project for fun and profit. Let's put some Haskell to work in browser.

I once attended course on Haskell in Wix. The course was great, but I slacked big time.
But the language is great with great thought behind it. I remember something, but not much.

## Progress

### Installation

Let's start with simple haskel progarm just to remember how this thing works.

Of course, start from the https://www.haskell.org/get-started/

They recommend to install [GHCup](https://www.haskell.org/ghcup/). I remember such thing.
First thing first, let's check if this thing is on `homebrew`. Yes, it is.

And GHCup has Ukrainian flag on top of the website, which is great.

So here we go, `brew install ghcup`.

And also there is a package manager for Haskell called `cabal`. There should be executable, but it's not there.

By calling `ghcup` without arguments, there is a list of commands. `install` is among them.

Let's try `ghcup install`. Works, gives warning that it's and old version. But still works, downlods something. Nice.

OMG, there is `ghcup tui` command that gives nice UI to pick/install/uninstall versions.

Ok, ok, now we have compiler, cabal and Haskell language server. Neat. Still no `cabal` command in the terminal.

Probably I should have done one-liner from the `ghcup` website. `wget | sh`. Owell, now need to do that manually.

There is a section with [manual installation](https://www.haskell.org/ghcup/install/#manual-installation).
I need to add Haskell stuff to PATH

```sh
export PATH="$HOME/.cabal/bin:$HOME/.ghcup/bin:$PATH"
```

I went a bit fancier and did

```sh
if [ -d "$HOME/.ghcup" ]; then
  path=($HOME/.ghcup/bin $path)
fi
if [ -d "$HOME/.cabal" ]; then
  path=($HOME/.cabal/bin $path)
fi
```

It loads `ghcup` binaries and `cabal` if only there is a directory in the home (where it is installed).
Now we have binaries. LET'S GO!

`ghci` launches the interpreter. Now let's write some Haskell code.

Oh, nice, we have our first program

```sh
ghc main.hs && ./main
# or
runghc main.hs
```

### Getting to know Haskell a bit more

Let's go over the book and try to do server with HTML rendering.

By [this book][blog].

Nah, this book is about Haskell and not webservers. Let's install local `express` via `cabal`.

Found [this thing](https://hackage.haskell.org/package/wai) that is called `wai`.

```sh
cabal init #
cabal update # update package list. they don't have autoupdate?
cabal install wai warp # for some reason does not work
```

Now I can run my Haskell program lik `cabal run`.

Ok, the way to install packages is to add them to the `.cabal` file in `build-depends` section
and then running `cabal install`.

### Running Haskell server

Let's continue. This is cool, as we already have a basic server on 3000 port.

I'm too procedural to understand how to render HTML using functional language.

But there is a good step-by-step [tutorial](https://jaspervdj.be/blaze/tutorial.html) on how to do that.
Package is called `blaze-html` and `blaze-<something>` in general. Let's try.

Now let's figure out how to serve HTML and JS file.

Nice, found the way to include `script` tag with `src` via

```hs
H.script ! A.src "bundle.js" $ ""
```

Now I see that my backend returns the same html on each request, need to add routing.
Let's not read from file, but serve raw string from code.

I know that there is such thing as pattern matching and there might be a way of somehow matching request with specific url.
But maaaan, I lack Haskell knowledge so hard.

Anyways, what is the `$` operator? Search says it's function application operator.

[Here](https://hackage.haskell.org/package/base-4.20.0.1/docs/Data-Function.html#v:-36-) are great examples:

```hs
strSum :: String -> Int
strSum s = sum (mapMaybe readMaybe (words s))
{- equals to -}
strSum s = sum $ mapMaybe readMaybe $ words s
```

HAHAHAHA, hell yeah, now we serve JS files from Haskell backend via `responseFile`.
There is a switch/case. I was randomly closing tabs and found [this page](https://wiki.haskell.org/Web/Servers) with examples.

_Fun fact, there is no electricity now in my apartment. Damn russians destroyed our power generation_

Ok, now we can create `WebAssembly` as in the [WASM MDN](https://developer.mozilla.org/en-US/docs/WebAssembly/JavaScript_interface/Module).
Why do we need a `Worker`? I'll try without it. (Spoiler, it works without service worker).

### WASM

Cool, initial JS binding is done. Let's try to create a `.wasm` build of Haskell file.
Probably will need another target in `.cabal` file or something.
Going through the [GHC WebAssembly guide](https://ghc.gitlab.haskell.org/ghc/doc/users_guide/wasm.html).

Now installing [ghc-wasm-meta](https://gitlab.haskell.org/ghc/ghc-wasm-meta) as in README.

Installed, it exports some executables and initial run of `wasm32-wasi-cabal run` didn't work for some reason.
Probably I miss something.

Meanwhile, found that I can add `default-extensions: OverloadedStrings` to the `.cabal` file and not to include weird LANGUAGE pragma at the top.

While I was on the break, I had an idea of pattern matching response. Let's try to implement it.

Neat, it worked. Instead of switch-case there is a function `router`.

```hs
router :: [Text] -> Response
router ["bundle.js"] = responseFile status200 [("Content-Type", "application/javascript")] "bundle.js" Nothing
router _ = responseLBS status200 [("Content-Type", "text/html")] $ renderHtml rootHtml
```

Also, took some time to discover what are monads and implement Maybe/Just/Nothing in typescript.
Still I don't feel confident, but at least its' moving.

Now, let's continue with packing some entrypoint in wasm module.

First of all, I think we need different executable in `.cabal` file. Fortunately, it supports it.

Created new entrypoint `counter`.

Apparently, some [wasi shim](https://github.com/bjorn3/browser_wasi_shim) (who knows what it is) is needed for it to work.
Also, [this thing](https://gitlab.haskell.org/ghc/ghc-wasm-meta#creating-a-wasi-reactor-module-from-wasm32-wasi-ghc) helped a lot.
And yes, we have now a Haskell server serving Haskell HTML and `.wasm` file and small JS binding. And we can use Haskell function from within JS code.
Basically, Haskell in browser.

## Learning

It feels like I have a big gap in Haskell knowledge, so I'll start with that [Haskell via sokoban][sokoban] tutorial.
And then will transition to [cis194][cis194].

## Semigroup

- non-empty set S;
- binary operation ⋅ (where ⋅ :: S → S → S);

That satisfies **Associativity**

For all a, b, c ∈ S, the equation (a ⋅ b) ⋅ c = a ⋅ (b ⋅ c) holds.

## Monoid

Semigroup (set S, operation ⋅, **Associativity**)

That has **Identity element**

There exists an element e in S such that for every element a in S,
the equalities e ⋅ a = a and a ⋅ e = a hold.

## Group

Monoid (set S, operation ⋅, **Associativity**, **Identity element**)

That has **Inverse element**

For each a in S, there exists an element b in G
such that a ⋅ b = e and b ⋅ a = e,
where e is the identity element.

## Abelian group

Group (set S, operation ⋅, **Associativity**, **Identity element**, **Inverse element**)

That satisfies **Commutativity**

For all a, b in S, a ⋅ b = b ⋅ a

There is also a **non-abelian/non-commutative group** that does not satisfy **Commutativity**.

## Ring

- set S with 2 binary operations + and ⋅;
- (S, +) is abelian group;
- (S, ⋅) is monoid;

That satisfies **Distributivity** of ⋅ with respect to +

- a · (b + c) = (a · b) + (a · c) for all a, b, c in R (left distributivity).
- (b + c) · a = (b · a) + (c · a) for all a, b, c in R (right distributivity).

## Commutative ring

Ring

Where · satisfies **Commutativity** (like abelian group, but without inverse element).

For all a, b in S, a ⋅ b = b ⋅ a

For example, integer numbers is a commutative ring.

## Field

Commutative ring

Where

- there exist two distinct elements 0 and 1 in S such that a + 0 = a and a ⋅ 1 = a.
- 0 ≠ 1
- all nonzero elements are invertible under multiplication;

## Monads

Monads are just monoids on the category of endofunctors.

## Endofuctor

A functor that maps a category to that same category; e.g., polynomial functor.

[blog]: https://learn-haskell.blog/03-html/01-html_content.html
[sokoban]: https://haskell-via-sokoban.nomeata.de/
[cis194]: https://www.cis.upenn.edu/~cis1940/fall16/
