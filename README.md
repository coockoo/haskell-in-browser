# Haskell in browser

This is a project for fun and profit. Let's put some Haskell to work in browser.

I once attended course on Haskell in Wix. The course was great, but I slacked big time.
But the language is great with great thought behind it. I remember something, but not much.

## Progress

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
ghc main.hs && ./main.hs
```
