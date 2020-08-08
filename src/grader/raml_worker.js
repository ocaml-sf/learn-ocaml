
onmessage = function (e){
	try {
		
		console.log("Domi1n:,end ");
		
		var xhttp = new XMLHttpRequest();
		xhttp.onreadystatechange = function() {
		    if (this.readyState == 4 && this.status == 200) {
		       // Typical action to be performed when the document is ready:
		       postMessage(xhttp.responseText);
		    }
		};
		xhttp.open("POST", "http://192.168.203.213:5000/", false);
		xhttp.overrideMimeType("text/html");
		xhttp.setRequestHeader('Content-Type', 'text/plain')
		xhttp.send(e.data);
	}catch (e) {
		postMessage('(fail')
	}

	console.log(e.data);


}


