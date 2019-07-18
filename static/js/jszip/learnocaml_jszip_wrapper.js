//keep in sync with editor export datatype
function editor_download(brut_data, callback) {
    var zip = new JSZip();
    var data = JSON.parse(brut_data);
    zip.file("descr.md", data.exercise.descr);
    zip.file("meta.json", JSON.stringify(data.metadata, null, 2));
    zip.file("prelude.ml", data.exercise.prelude);
    zip.file("prepare.ml", data.exercise.prepare);
    zip.file("template.ml", data.exercise.template);
    zip.file("test.ml", data.exercise.test);
    zip.file("solution.ml", data.exercise.solution);
    zip.generateAsync({
        type: "blob",
        compression: "DEFLATE",
        compressionOptions: {
            level: 9
        }
    }).then(function(blob) { callback(blob) });
}


//also to keep in sync
function editor_import(brut_data, callback) {
    var zip = new JSZip();
    zip.loadAsync(brut_data)
        .then(function(loaded_zip) {
            var result = { exercise: {}, metadata: {} };

            var descr = loaded_zip.file("descr.md").async("string");
            var meta = loaded_zip.file("meta.json").async("string");
            var prelude = loaded_zip.file("prelude.ml").async("string");
            var prepare = loaded_zip.file("prepare.ml").async("string");
            var template = loaded_zip.file("template.ml").async("string");
            var test = loaded_zip.file("test.ml").async("string");
            var solution = loaded_zip.file("solution.ml").async("string");
            Promise.all([descr, meta, prelude, prepare, template, test, solution])
                .then(function(values) {
                    result.exercise.max_score = 0;
                    result.exercise.id = "";
                    result.exercise.descr = values[0];
                    var brut_meta = values[1];
                    var meta = brut_meta.replace(/\r?\n|\r/g, " ");
                    result.metadata = JSON.parse(meta);
                    result.exercise.prelude = values[2];
                    result.exercise.prepare = values[3];
                    result.exercise.template = values[4];
                    result.exercise.test = values[5];
                    result.exercise.solution = values[6];
                    console.log(brut_data);
                    callback(JSON.stringify(result));
                });
        });
}