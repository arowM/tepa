// @ts-check
const { Elm } = require('../App.elm');

Elm.App.init({
  node: document.body.appendChild(document.createElement('div')),
  flags: {}
});
