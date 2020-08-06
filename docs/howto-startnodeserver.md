How to start the Node server  
=========================================
This section explains how to start the Node.js app that serves the backend to collect student data. The app is located in the main 
learn-ocaml directory in /app.js . The app uses four environment variables which must be specified at run time.
PORT specifies which port the Node app will run on. DB_CONN refers to the mongo database to which the Node server will send data. COMP_COLLECTION and EVAL_COLLECTION specify the collections that will host data collected on 'Compile' and 'Eval', respectively. 

* Run the Node app locally on port 8000: 
``` 
PORT=8000 DB_CONN='mongodb://localhost/learn-ocaml-code' COMP_COLLECTION='compiled_code' EVAL_COLLECTION='eval_code' node app.js

```

# Environment Variables 
There are two environment variables that are not specified at run time, but before deploying. They exist in static/js/get-local-changes.js and src/toplevel/get-eval.js . In these two files, a variable ``` path ``` is declared to specify where the Node app is running. For example, if we have a local Node app listening on port 8000 

* To collect code when Compile is clicked
In static/js/get-local-changes.js, we declare ``` const path = "//localhost:8000" ``` .

* To collect code when Eval Code is clicked
In static/js/get-local-changes.js, we declare ``` const path = "//localhost:8000/eval" ``` .
Note that the path will always end in /eval to reflect the endpoint defined in app.js.

