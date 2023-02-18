module.exports = {
  "env": {
    "node": true,
    "es6": true
  },
  "extends": [
    "eslint:recommended",
    "plugin:@typescript-eslint/recommended",
    "prettier"
  ],
  "plugins": [
    "@typescript-eslint"
  ],
  "parser": "@typescript-eslint/parser",
  "parserOptions": {
    "sourceType": "module",
    "project": "./tsconfig.json"
  },
  "root": true,
  "rules": {
    "@typescript-eslint/strict-boolean-expressions": [
      2,
      {
        "allowString" : false,
        "allowNumber" : false
      }
    ],
    "@typescript-eslint/no-unused-vars": [
      "warn", { "argsIgnorePattern": "^_" }
    ],
    "no-constant-condition": ["error", { "checkLoops": false }]
  }
}
