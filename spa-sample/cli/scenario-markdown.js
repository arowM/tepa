const { Elm } = require("../dist/scenario-worker.js");
const fs = require("fs");

const app = Elm.ScenarioWorker.init({
  flags: {
    dev: true,
  },
});

app.ports.output_request.subscribe((req) => {
  if (!req.body.value) {
    throw req.body.error;
  } else {
    fs.writeFileSync("./dist/scenario.md", req.body.value);
    console.log("Save scenario as ./dist/scenario.md");
  }
  app.ports.output_response.send({ ...req, body: null });
});
