## Get Zapped

Hair removal, especially electrolysis, costs a lot. It's also incredibly important for many trans people. In 2017 a group of trans organisers in Manchester responded to the massive community need for affordable electrolysis by crowdfunding clinics and training therapists. They worked to achieve a pay-as-you-can-afford service, delivered by people who understood the needs of trans people.

Existing booking systems are often expensive, and aren't designed for the lived experience of trans people. So we wanted to build our own system from the base up. We knew that we could provide a more meaningful, equitable service by acting co-operatively - so we extended that approach to our web app.

Our vision is to develop a [platform co-operative](https://platform.coop/about) for electrolysis therapists. Workers co-ops and housing co-ops provided a significant portion of the funding for the electrolysis project to happen. That said, the nature of clinical work, especially when it is widely distributed geographically, makes it harder to start as co-operative businesses. Having a platform co-op, that is owned by the therapists, allows for co-operation and mutual benefit between them.

We are currently in the initial stages of developing a minimum viable product. We hope that once we have that we can reach out for funding support, and to therapists outside of Manchester.

To find out more about the electrolysis therapists:
* [Read this interview with Siobhan in Vice](https://broadly.vice.com/en_us/article/ev7a7m/permanent-hair-removal-for-trans-women)
* [Tortoise Beats Hair](https://www.tortoisebeatshair.com/)
* [Electrolysis by Siobhan](https://www.electrolysisbysiobhan.co.uk)


## Database Setup

After installing Postgres, run:

```
createuser get-zapped --password get-zapped --superuser
createdb get-zapped
createdb get-zapped_test
```

## Haskell Setup

1. If you haven't already, [install Stack](https://haskell-lang.org/get-started)
	* On POSIX systems, this is usually `curl -sSL https://get.haskellstack.org/ | sh`
2. Install the `yesod` command line tool: `stack install yesod-bin --install-ghc`
3. Build libraries: `stack build`

If you have trouble, refer to the [Yesod Quickstart guide](https://www.yesodweb.com/page/quickstart) for additional detail.

## Development

Start a development server with:

```
yesod devel
```

As your code changes, your site will be automatically recompiled and redeployed to localhost.

## Tests

```
stack test --flag get-zapped:library-only --flag get-zapped:dev
```

(Because `yesod devel` passes the `library-only` and `dev` flags, matching those flags means you don't need to recompile between tests and development, and it disables optimization to speed up your test compile times).

## Documentation

* Read the [Yesod Book](https://www.yesodweb.com/book) online for free
* Check [Stackage](http://stackage.org/) for documentation on the packages in your LTS Haskell version, or [search it using Hoogle](https://www.stackage.org/lts/hoogle?q=). Tip: Your LTS version is in your `stack.yaml` file.
* For local documentation, use:
	* `stack haddock --open` to generate Haddock documentation for your dependencies, and open that documentation in a browser
	* `stack hoogle <function, module or type signature>` to generate a Hoogle database and search for your query
* The [Yesod cookbook](https://github.com/yesodweb/yesod-cookbook) has sample code for various needs

## Getting Help

* Ask questions on [Stack Overflow, using the Yesod or Haskell tags](https://stackoverflow.com/questions/tagged/yesod+haskell)
* Ask the [Yesod Google Group](https://groups.google.com/forum/#!forum/yesodweb)
* There are several chatrooms you can ask for help:
	* For IRC, try Freenode#yesod and Freenode#haskell
	* [Functional Programming Slack](https://fpchat-invite.herokuapp.com/), in the #haskell, #haskell-beginners, or #yesod channels.
