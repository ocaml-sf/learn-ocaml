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
    var dirname = data.exercise.id
    zip.folder(dirname);
    editor_create_exercise(zip, data, dirname + "/");
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
        var prefix = k + "/";
        editor_create_exercise(zip, all_data[k], prefix)
    });
    var index = JSON.stringify(JSON.parse(brut_index), null, 2);
    zip.file("index.json", index);
    zip.generateAsync({
        type: "blob",
        compression: "DEFLATE",
        compressionOptions: {
            level: 9
        }
    }).then(function(blob) { callback(blob) });
}

function editor_read_exercise(loaded_zip, path) {
    return new Promise(function(resolve, reject) {
        var descr = loaded_zip.file(path + "descr.md").async("string");
        var meta = loaded_zip.file(path + "meta.json").async("string");
        var prelude = loaded_zip.file(path + "prelude.ml").async("string");
        var prepare = loaded_zip.file(path + "prepare.ml").async("string");
        var template = loaded_zip.file(path + "template.ml").async("string");
        var test = loaded_zip.file(path + "test.ml").async("string");
        var solution = loaded_zip.file(path + "solution.ml").async("string");
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
                resolve(result);
            })
    })
}
/*
//also to keep in sync
function editor_import(brut_data, callback) {
    var zip = new JSZip();
    zip.loadAsync(brut_data)
        .then(function(loaded_zip) {
                if (loaded_zip.file("index.json")) {
                    loaded_zip.forEach(function(relative_path, file) {
                            if (file.dir) {
                                new Promise(function(resolve, reject) {
                                    editor_read_exercise(loaded_zip, relative_path)
                                        .then(function(result) {

                                        })
                                })
                            }
                        }
                    }
                    var result = { exercise: {}, metadata: {} };

                    callback(JSON.stringify(result));
                });
        });
}*/