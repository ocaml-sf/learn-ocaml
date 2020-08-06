track_local_changes();

function track_local_changes() {
    const request = new XMLHttpRequest();
    const path = "nodejs:8000"; // server ip and port number
    try {
        if (typeof (document.getElementsByClassName("ace_layer ace_text-layer")[0].innerHTML) !== 'undefined') {
            request.open("POST", path, true); // true = asynchronous
            request.setRequestHeader("Content-Type", "application/json; charset=UTF-8");
            // innerHTML contains nested HTML tags which encapsulate the code 
            const exercise = document.getElementsByClassName("ace_layer ace_text-layer")[0].innerHTML
            // strip of HTML to get raw code 
            const stringSolution = strip(exercise)
            // get student nickname 
            const nickname = document.getElementById("learnocaml-nickname").innerHTML
            // create Object 
            const obj = new Object();
            obj.nickname = nickname;
            obj.timestamp = Date.now();
            obj.solution = stringSolution;
            const jsonString = JSON.stringify(obj);
            // send to Database
            request.send(jsonString);
        }
    }
    catch (e) {
        console.log(e);
    }
}
    function strip(html) {
        var tmp = document.createElement("DIV");
        tmp.innerHTML = html
        return tmp.textContent || tmp.innerText || ""
    }