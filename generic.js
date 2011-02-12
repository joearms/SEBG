var websocket;
var output;  
var c;
var logdiv;

window.onload = 
    function() {
	logdiv = document.getElementById('log');
	log('<p>started');
    }

    function loadScript(File) {
	var s = document.createElement('script');
	s.type = 'text/javascript';
	s.src = File;
	s.onload = function(){send('loaded')};
	document.body.appendChild(s);
  }

  function onClose(evt) {
      log('<p><b>closed</b>');
      document.body.style.backgroundColor='#aabbcc';
  }  
  
  function onMessage(evt) {
      try
      {
	  eval(evt.data);
      }
      catch(e)
      {
	  alert("oops:"+evt.data);
      }
  }
  
  function onError(evt) { 
      document.body.style.backgroundColor='orange';
  }  
  
  function send(msg) {
      websocket.send(msg);
  }
  
  function log(x) {
      logdiv.innerHTML += x;
  }
  
  function start_session(wsUri){
      log("<br>try to connect");
      document.body.style.backgroundColor='#ccaabb';
      websocket = new WebSocket(wsUri); 
      websocket.onopen = function(evt) { onOpen(evt) }; 
      websocket.onclose = function(evt) { onClose(evt) }; 
      websocket.onmessage = function(evt) { onMessage(evt) }; 
      websocket.onerror = function(evt) { onError(evt) };
      return(false);
    }  
    
    function onOpen(evt) { 
	log("connected");
    }

