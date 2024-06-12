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

By [this book](https://learn-haskell.blog/03-html/01-html_content.html)

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
