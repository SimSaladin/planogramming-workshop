# Planogramming - Haskell/Elm workshop

- `shared` - The types (and API) shared between server and client code.
- `client` - Elm client (stub).
- `code-generator` - Create Elm bindings.
- `server` - Server code.
- `hs-client` - Haskell CLI client (stub).

## Elm client

Ensure that Elm 0.18 is installed in PATH, and execute:
```
cd client
elm make src/Main.elm
```
Open up `client/index.html` in a browser.

### What then?

- client/src/Api.elm - generated file, defines Api - don't modify.
- client/src/MAin.elm - main entry point (probably don't need to modify)
- client/src/Model.elm - the model and messages (see Elm documentation for what is model or message in FRP)
- client/src/State.elm - update function(s) - what is done when new message is received
- client/src/View.elm - view functions (what is shown on the page)

## Haskell client

Source: `hs-client/`.

Ensure that `stack` is installed and in PATH, and execute:
```
stack setup
stack build
```
Testing
```
stack exec planogramming-hs-client planogramming-workshop.relexsolutions.com 80
> Identify "test"
> GetProducts
```

## The server and bindings

Create client (Elm) bindings:
```
stack exec planogramming-code-generator
```

Start server
```
stack exec planogramming-server 3050
```

### Generate products from RELEX

```ruby
products = Hash.new;
ProductLocation.where("products.width > 0 && purchase_price > 0").map { |pl|
   r = Hash.new;
   p = Product.find(pl.product_id);
   r["ident"] = Integer(p.code);
   r["width"] = Integer(p.width);
   r["priceBuy"] = pl.purchase_price;
   r["priceSell"] = pl.sales_price;
   r["attributes"] = [];
   products[p.code] = r if products[p.code].nil?;
};
File.write("/tmp/products.json", products.values.to_json)
```

```haskell
-- stack ghci --package lens-aeson --package lens --package random --package vector
:set -XOverloadedStrings
:set -XOverloadedLists
:m Data.Aeson.Lens Control.Monad Control.Lens System.Random
import Data.Aeson (Value(..))
import qualified Data.List as L
import qualified Data.Vector as V

b <- readFile "products.json"
let len      = length (b ^.. values)
    genAttrs = fmap (L.sort . L.nub) $ replicateM 5 (randomRIO (1, 10))
attrs <- replicateM len genAttrs
let setAt bs i = bs & nth i . key "attributes" . _Array .~ V.fromList (map (Number . fromInteger) $ attrs !! i)
writeFile "products_out.json" (foldl setAt b [0..len-1])
```

### Server production build and deploy

```bash
stack docker pull
stack install --docker --local-bin-path=./deploy
docker build -t simsaladin/planogramming-server deploy

# Run local test
docker run -it --rm -p 3050:3050 simsaladin/planogramming-server

# Push
docker push simsaladin/planogramming-server

# Pull and run (target host)
docker pull simsaladin/planogramming-server
docker run -d -p 80:3050 simsaladin/planogramming-server
```
