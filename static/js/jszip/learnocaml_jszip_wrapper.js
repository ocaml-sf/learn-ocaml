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