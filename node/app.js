const express = require('express');
const bodyParser = require('body-parser');
const app = express();
const database = process.env.DB_CONN;
const port = process.env.PORT;
const compile_collection = process.env.COMP_COLLECTION;
const eval_collection = process.env.EVAL_COLLECTION;
const mongoose = require('mongoose');

app.use(bodyParser.text({ type: "application/json" }));

// connect to database
// mongoose.connect('mongodb://172.17.0.1/learn-ocaml-code');
mongoose.connect(database, { useNewUrlParser: true, useUnifiedTopology: true} );
const db = mongoose.connection;
db.on('error', console.error.bind(console, 'MongoDB connection error:'));

// Access Control
app.use(function(req, res, next) {
        res.setHeader('Access-Control-Allow-Origin', '*');
        res.setHeader('Access-Control-Allow-Methods', 'GET, POST, OPTIONS, PUT, PATCH, DELETE');
        res.setHeader('Access-Control-Allow-Headers', '*');
    next();
  });


// receive the POST from the client javascript file
app.post("/", function (req, res)
{
  if (req.body)
  {
    const solution = JSON.parse( req.body ); // parse req.body as an object
    db.collection(compile_collection).insertOne(solution);
    console.log(solution);
    res.sendStatus(200); // success status
  }
  else
  {
    res.sendStatus(400); // error status
  }
});  

app.post("/eval", function (req, res)
{
  if (req.body)
  {
    const solution = JSON.parse( req.body ); // parse req.body as an object
    db.collection(eval_collection).insertOne(solution);
    console.log(solution);
    res.sendStatus(200); // success status
  }
  else
  {
    res.sendStatus(400); // error status
  }
});  



app.listen(port, () => {
  console.log(`Server running on port${port}`);
});