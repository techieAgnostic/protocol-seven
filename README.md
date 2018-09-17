# Regular Flolloping

Post-Netrunner Rotation, currently hosted at [Protocol Seven](https://anr.p7.co.nz)

## Getting Started

What you need to get the generator up and running.

### Prerequisites

Built using Nix.
Nix can be installed with:  
```
curl https://nixos.org/nix/install | sh
```  

### Dependencies

`imagemagick` is required for the favicons.

### Installing

Compile the generator

```
nix-build p7.nix
```  

Enter the build environment

```
nix-shell --pure
```

Generate the site

```
./result/site rebuild
```

And test it out

```
./result/site watch
```

The site will now be avaliable at `localhost:8000`

## Deployment

Site will be completely static, so simply point your server to the `_site` directory

## Built With

* [Hakyll](https://jaspervdj.be) - The web framework used
* [hakyll-favicon](https://github.com/elaye/hakyll-favicon) - Thanks Elie!
* [Nix](https://nixos.org) - Package Management
* [Cabal](https://cabal.readthedocs.io) - Build System

## Versioning

Is very airy fairy and mainly based on what I think constitutes major / minor updates.

## Authors

* **Shaun Kerr** - [tA](https://github.com/techieAgnostic)

## License

This project is licensed under the BSD3 License - see the [LICENSE](LICENSE) file for details

## Acknowledgments

* Chris Hay for coming up with the idea
* Elie Génard for their favicon library, very easy to use.
* Serial Experiments Lain for being amazing and the source of the name
* ***NOT*** Wizards of the Coast, because those [REDACTED] cancelled the game >:[
* You, for reading this :)
