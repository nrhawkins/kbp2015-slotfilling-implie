// Localhost.
window.onload = function() {
  document.getElementById("submit").addEventListener("click",
    submit,
    false
  ); 
};

var url = "http://127.0.0.1:8080/";

function submit() {
  console.log("In form submit");
  var form = document.getElementById('form');
  var sentence = form.elements['sentence'].value; 
  var params = "sentence=" + encodeURIComponent(sentence);
  writeResults(params);
  var response = httpGet(url + "?" + params);
  alert(response);
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
