const { Elm } = require("../dist/scenario-worker.js");
const fs = require("fs");

const app = Elm.ScenarioWorker.init({
  flags: {
    "dev": true
  }
});

app.ports.output.subscribe((message) => {
  fs.writeFileSync("./dist/scenario.md", message);
});
