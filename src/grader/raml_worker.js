
onmessage = function (e){
	try {
		var xhttp = new XMLHttpRequest();
		xhttp.onreadystatechange = function() {
		    if (this.readyState == 4 && this.status == 200) {
		       postMessage(xhttp.responseText);
		    }
		};
		xhttp.open("POST", e.data[1], false);
		xhttp.overrideMimeType("text/html");
		xhttp.setRequestHeader('Content-Type', 'text/plain')
		xhttp.send(e.data[0]);
	}catch (e) {
		postMessage('(fail')
	}



}


