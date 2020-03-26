# liquidity-js

A pure JavaScript [Liquidity](https://liquidity-lang.org) compiler and client.

This library can be used either with NodeJs or in the browser.

## Installation

From then NPM repository:

```
npm install liquidity-js
```

Install the latest version from this repository:

```
npm install OCamlPro/liquidity-js
```

Alternatively, the minified JavaScript files are also available in the
[dist](./dist) directory:

- for NodeJs use [liquidity.node.js](./dist/liquidity.node.js)
- for web browsers use [liquidity.browser.js](./dist/liquidity.browser.js)

## Usage

### As a Node Module

```javascript
const liquidity = require("liquidity-js");
(async () => {

    await liquidity.ready;

    // Setting options
    liquidity.options.private_key = "edsk3impUREDjtAvDvz8MWQsY7JQyFHhBfVoU6CztBUCLw3ocuqqJ7";
    liquidity.options.node = "http://testnet-node.dunscan.io";
    
    // Compile
    var my_contract = 
        `type storage = int
         let%init storage x = 2 * (x:int) + 1
         let%entry default () x = [], x + 1`;
    console.log(liquidity.compiler.compile(my_contract));

    // Deploy contract
    var op = await liquidity.client.deploy({
        code: my_contract,
        arguments: ["1"]
    });
    console.log("Operation hash:", op.operation_hash);
    console.log("Originated contract:", op.contract);
    
})().catch(console.error);
```

### In a Web Browser

```html
<script src="./liquidity.browser.js" type="text/javascript" async></script>
<script type="text/javascript" async>
  window.onload = async () => {
    await liquidity.ready;

    // Setting options
    liquidity.options.private_key = "edsk3impUREDjtAvDvz8MWQsY7JQyFHhBfVoU6CztBUCLw3ocuqqJ7";
    liquidity.options.node = "http://testnet-node.dunscan.io";

    // Compile
    var my_contract = 
      `type storage = int
       let%init storage x = 2 * (x:int) + 1
       let%entry default () x = [], x + 1`;
    console.log(liquidity.compiler.compile(my_contract));

    // Deploy contract
    liquidity.client.deploy({
      code: my_contract,
      arguments: ["1"]
    }).then(console.log).catch(console.error);

  }
</script>
```
