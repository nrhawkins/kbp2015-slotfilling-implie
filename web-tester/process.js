// Localhost.
window.onload = function() {
  document.getElementById("submit").addEventListener("click",
    submit,
    false
  ); 
};

//var url = "http://128.208.6.92:8080/";
//var url = "tricycle.cs.washington.edu:8080/";
var url = "http://reliable.cs.washington.edu/implie";

function submit() {
  console.log("In form submit");
  var form = document.getElementById('form');
  var sentence = form.elements['sentence'].value; 
  var params = "sentence=" + encodeURIComponent(sentence);
  writeResults(params);
  var response = httpGet(url + "?" + params);
  writeResults(response);
}

function writeResults(responseText) {
  document.getElementById("tagging").innerHTML = responseText;
}

function httpGet(theUrl) {
  var xmlHttp = null;
  xmlHttp = new XMLHttpRequest();
  xmlHttp.open("GET", theUrl, false);
  xmlHttp.send(null);
  return xmlHttp.responseText;
}
