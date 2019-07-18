//keep in sync with editor export datatype
function editor_create_exercise(zip, data, prefix) {
    zip.file(prefix + "descr.md", data.exercise.descr);
    zip.file(prefix + "meta.json", JSON.stringify(data.metadata, null, 2));
    zip.file(prefix + "prelude.ml", data.exercise.prelude);
    zip.file(prefix + "prepare.ml", data.exercise.prepare);
    zip.file(prefix + "template.ml", data.exercise.template);
    zip.file(prefix + "test.ml", data.exercise.test);
    zip.file(prefix + "solution.ml", data.exercise.solution);
}

function editor_download(brut_data, callback) {
    var zip = new JSZip();
    var data = JSON.parse(brut_data);
    editor_create_exercise(zip, data, "")
    zip.generateAsync({
        type: "blob",
        compression: "DEFLATE",
        compressionOptions: {
            level: 9
        }
    }).then(function(blob) { callback(blob) });
}

function editor_download_all(brut_exercises, brut_index, callback) {
    var zip = new JSZip();
    var all_data = JSON.parse(brut_exercises);
    Object.keys(all_data).forEach(function(k) {
        zip.folder(k);
        let prefix = k + "/";
        editor_create_exercise(zip, all_data[k], prefix)
    });
    zip.file("index.json", brut_index);
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